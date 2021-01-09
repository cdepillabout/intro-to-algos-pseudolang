
module Pseudolang.Lexer where

import Pseudolang.Prelude hiding (many, some, try)

import Text.Megaparsec (ParsecT, Pos, SourcePos, choice, getOffset, many, notFollowedBy, some, try)
import Text.Megaparsec.Char (alphaNumChar, asciiChar, hspace1, string)
import qualified Text.Megaparsec.Char.Lexer as Megaparsec.Lexer

type Parser = ParsecT Void Text Identity

data Token = Token
  { token :: Tok
  , startPos :: Int
  , endPos :: Int
  } deriving (Eq, Show)

data Tok
  = TokCloseCurlyBrace
  | TokCloseParen
  | TokCloseSquareBracket
  | TokComma
  | TokDownTo
  | TokEquals
  | TokFor
  | TokGreaterThan
  | TokIdentifier Text
  | TokIndent Pos -- ^ How many spaces this indent includes.
  | TokInfinity
  | TokLessThan
  | TokMinus
  | TokNewline
  | TokOpenCurlyBrace
  | TokOpenParen
  | TokOpenSquareBracket
  | TokPlus
  | TokReturn
  | TokTo
  deriving (Eq, Show)

tokenizer :: Parser [Token]
tokenizer = some lexer

lexer :: Parser Token
lexer =
  reservedSymsParser <|> try reservedAlphaWordsParser <|> identifierParser

space :: Parser ()
space = Megaparsec.Lexer.space hspace1 empty empty

lexeme :: Parser a -> (a -> Tok) -> Parser Token
lexeme p f = do
  startingOffset <- getOffset
  res <- p
  endingOffset <- getOffset
  space
  pure $ Token (f res) startingOffset endingOffset

-- symbol :: Text -> Tok -> Parser Token
-- symbol text tok = do
--   startingOffset <- getOffset
--   void $ string text
--   endingOffset <- getOffset
--   notFollowedBy (
--   space
--   pure $ Token tok startingOffset endingOffset

-- symEqualsParser :: Parser Token
-- symEqualsParser = symbol "=" TokEquals

reservedAlphaWords :: [(Text, Tok)]
reservedAlphaWords =
  [ ("for", TokFor)
  , ("downto", TokDownTo)
  ]

-- reservedAlphaWordsParser :: Parser Token
-- reservedAlphaWordsParser = do
--   startingOffset <- getOffset
--   tok <- choice $ fmap (\(str, tok) -> string str $> tok) reservedAlphaWords
--   endingOffset <- getOffset
--   notFollowedBy alphaNumChar
--   space
--   pure $ Token tok startingOffset endingOffset
reservedAlphaWordsParser :: Parser Token
reservedAlphaWordsParser = do
  startingOffset <- getOffset
  tok <- choice $ fmap (\(str, tok) -> string str $> tok) reservedAlphaWords
  endingOffset <- getOffset
  notFollowedBy alphaNumChar
  space
  pure $ Token tok startingOffset endingOffset

reservedSyms :: [(Text, Tok)]
reservedSyms =
  [ ("=", TokEquals)
  , ("<", TokLessThan)
  ]

reservedSymsParser :: Parser Token
reservedSymsParser = do
  startingOffset <- getOffset
  tok <- choice $ fmap (\(str, tok) -> string str $> tok) reservedSyms
  endingOffset <- getOffset
  space
  pure $ Token tok startingOffset endingOffset

identifierParser :: Parser Token
identifierParser = do
  startingOffset <- getOffset
  firstChar <- asciiChar
  (remainingChars :: [Char]) <- many alphaNumChar
  endingOffset <- getOffset
  space
  pure $ Token (TokIdentifier $ pack (firstChar : remainingChars)) startingOffset endingOffset
