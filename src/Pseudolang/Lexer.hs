
module Pseudolang.Lexer where

import Pseudolang.Prelude hiding (some)

import Text.Megaparsec (ParsecT, Pos, SourcePos, getOffset, some)
import Text.Megaparsec.Char (hspace1, string)
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
  symForParser <|> symEqualsParser

space :: Parser ()
space = Megaparsec.Lexer.space hspace1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Megaparsec.Lexer.lexeme space

symbol :: Text -> Tok -> Parser Token
symbol text tok = do
  startingOffset <- getOffset
  void $ string text
  endingOffset <- getOffset
  space
  pure $ Token tok startingOffset endingOffset

symForParser :: Parser Token
symForParser = symbol "for" TokFor

symEqualsParser :: Parser Token
symEqualsParser = symbol "=" TokEquals
