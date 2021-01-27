{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Pseudolang.Utils where

import Pseudolang.Prelude

import qualified Language.Haskell.Exts.Extension as Ext
import Language.Haskell.Exts.Parser (ParseMode(..), ParseResult(..), defaultParseMode, parseExpWithMode)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup (Min(..))
import Data.String.Interpolate.Conversion (build, finalize, interpolate, ofString)
import Data.String.Interpolate.Parse (InterpSegment(..), dosToUnix, parseInterpSegments)
import Data.List.Split (dropDelims, split, whenElt)
import Language.Haskell.Meta (toExp)
import Language.Haskell.TH (Exp, Q, extsEnabled)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

_i :: QuasiQuoter
_i = QuasiQuoter
  { quoteExp  = toExp . parseInterpSegments . dosToUnix
  , quotePat  = const $ errQQType "__i" "pattern"
  , quoteType = const $ errQQType "__i" "type"
  , quoteDec  = const $ errQQType "__i" "declaration"
  }
  where toExp :: Either String [InterpSegment] -> Q Exp
        toExp parseResult = case parseResult of
          Left msg   -> errQQ "__i" msg
          Right segs -> unindent segs >>= interpToExp

        unindent :: [InterpSegment] -> Q [InterpSegment]
        unindent segs =
          let lines = interpLines segs
              mindent = mindentation lines
          in pure $! (interpUnlines . reduceIndents mindent) lines

interpLines :: [InterpSegment] -> [[InterpSegment]]
interpLines = split $ dropDelims $ whenElt (== Newline)

interpUnlines :: [[InterpSegment]] -> [InterpSegment]
interpUnlines = intercalate [Newline]

interpToExp :: [InterpSegment] -> Q Exp
interpToExp = outputToExp . collapseStrings . renderOutput

data Mindent = UsesSpaces Int | UsesTabs Int

mindentation :: [[InterpSegment]] -> Mindent
mindentation lines =
  let nonblank = filter (not . blankLine) lines
      withIndent = find (\case { Spaces _ : _ -> True; Tabs _ : _ -> True; _ -> False }) nonblank
  in case withIndent of
      Nothing -> UsesSpaces 0
      Just (Spaces _ : _) ->
        maybe (UsesSpaces 0) UsesSpaces $
          findMinIndent (\case { Spaces n -> Just n; _ -> Nothing }) Nothing nonblank
      Just (Tabs _ : _) ->
        maybe (UsesSpaces 0) UsesTabs $
          findMinIndent (\case { Tabs n -> Just n; _ -> Nothing }) Nothing nonblank
      Just _ -> UsesSpaces 0
  where
    findMinIndent :: (InterpSegment -> Maybe Int) -> Maybe Int -> [[InterpSegment]] -> Maybe Int
    findMinIndent _ found [] = found
    findMinIndent f found ((seg:_):rest) =
      findMinIndent f (getMin <$> on mappend (fmap Min) (f seg) found) rest
    findMinIndent f found ([]:rest) = findMinIndent f found rest

reduceIndents :: Mindent -> [[InterpSegment]] -> [[InterpSegment]]
reduceIndents _ [] = []
reduceIndents i@(UsesSpaces indent) ((Spaces n:line):rest) =
  (Spaces (n-indent):line) : reduceIndents i rest
reduceIndents i@(UsesTabs indent) ((Tabs n:line):rest) =
  (Tabs (n-indent):line) : reduceIndents i rest
reduceIndents i (line:rest) = line : reduceIndents i rest

blankLine :: [InterpSegment] -> Bool
blankLine [] = True
blankLine (Expression _ : _) = False
blankLine (Newline : rest) = blankLine rest
blankLine (Spaces _ : rest) = blankLine rest
blankLine (Tabs _ : rest) = blankLine rest
blankLine (Verbatim str:rest) = blank str && blankLine rest
  where
    blank :: String -> Bool
    blank = all (\c -> elem c [' ', '\t'])

errQQ :: MonadFail m => String -> String -> m a
errQQ qqName msg =
  fail ("Data.String.Interpolate." ++ qqName ++ ": " ++ msg)

errQQType :: MonadFail m => String -> String -> m a
errQQType qqName = errQQ qqName . ("This QuasiQuoter cannot be used as a " ++)


data OutputSegment
  = OfString String
  | Interpolate String

collapseStrings :: [OutputSegment] -> [OutputSegment]
collapseStrings [] = []
collapseStrings (OfString s1 : OfString s2 : rest) =
  collapseStrings ((OfString $ s1 ++ s2) : rest)
collapseStrings (other : rest) = other : collapseStrings rest

renderOutput :: [InterpSegment] -> [OutputSegment]
renderOutput = fmap renderSegment
  where renderSegment :: InterpSegment -> OutputSegment
        renderSegment (Verbatim str)   = OfString str
        renderSegment Newline          = OfString "\n"
        renderSegment (Spaces n)       = OfString (replicate n ' ')
        renderSegment (Tabs n)         = OfString (replicate n '\t')
        renderSegment (Expression str) = Interpolate str

outputToExp :: [OutputSegment] -> Q Exp
outputToExp segs = [|finalize Proxy $(go segs)|]
  where
    renderExp :: OutputSegment -> Q Exp
    renderExp (OfString str) = [|ofString Proxy str|]
    renderExp (Interpolate expr) = [|interpolate Proxy $(reifyExpression expr)|]

    go :: [OutputSegment] -> Q Exp
    go =
      foldr
        (\seg qexp -> [|build Proxy $(renderExp seg) $(qexp)|])
        [|ofString Proxy ""|]

reifyExpression :: String -> Q Exp
reifyExpression s = do
  -- We want to explicitly use whatever extensions are enabled in current module
  exts      <- (fmap . fmap) (Ext.parseExtension . show) extsEnabled
  parseMode <- pure (defaultParseMode { extensions = exts })
  case parseExpWithMode parseMode s of
    ParseFailed _ err  -> fail $
      "Data.String.Interpolate.i: got error: '" ++ err ++ "' while parsing expression: " ++ s
    ParseOk e -> pure (toExp e)
