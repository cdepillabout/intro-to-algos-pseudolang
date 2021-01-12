
module Test.Pseudolang.ParserTest where

import Pseudolang.Prelude

import Text.Megaparsec (eof, errorBundlePretty, mkPos, parse)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Pseudolang.Lexer (Tok(..), Token(Token))
import Pseudolang.Parser

parserTest ::
     ( Eq a
     , Show a
     -- , VisualStream [Token]
     )
  => Parser a
  -> [Token]
  -> a
  -> IO ()
parserTest parser input expectedRes = do
  let eitherRes = parse (parser <* eof) "" input
  case eitherRes of
    Right res -> res `shouldBe` expectedRes
    Left err -> expectationFailure $ show err

mkTok :: Tok -> Token
mkTok tok = Token tok 0 0

mkToks :: [Tok] -> [Token]
mkToks = fmap mkTok

test :: Spec
test =
  describe "Parser" $ do
    it "test1" $ do
      parserTest identParser [mkTok (TokIdentifier "foo")] (Identifier "foo")
    it "test2" $ do
      let inputTokens =
            [ TokIdentifier "foo"
            , TokPlus
            , TokIdentifier "bar"
            ]
          expectedAST = ExprPlus (ExprVar (Identifier "foo")) (ExprVar (Identifier "bar"))
      parserTest exprParser (mkToks inputTokens) expectedAST
    it "test3" $ do
      let inputTokens =
            [ TokInteger 3
            , TokPlus
            , TokInteger 4
            , TokTimes
            , TokInteger 5
            ]
          expectedAST =
            ExprPlus
              (ExprInteger 3)
              (ExprTimes
                (ExprInteger 4)
                (ExprInteger 5)
              )
      parserTest exprParser (mkToks inputTokens) expectedAST
