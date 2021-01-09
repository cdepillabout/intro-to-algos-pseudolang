
module Pseudolang.Lexer where

import Pseudolang.Prelude hiding (some)

import Text.Megaparsec (ParsecT, Pos, SourcePos, some)
import Text.Megaparsec.Char (hspace1)
import qualified Text.Megaparsec.Char.Lexer as Megaparsec.Lexer

type Parser = ParsecT Void Text Identity

data Token = Token
  { token :: Tok
  , position :: SourcePos
  , span :: Pos
  }

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

tokenizer :: Parser [Token]
tokenizer = some lexer

lexer :: Parser Token
lexer = undefined

space :: Parser ()
space = Megaparsec.Lexer.space hspace1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Megaparsec.Lexer.lexeme space

symbol :: Text -> Tok -> Parser Tok
symbol text tok = Megaparsec.Lexer.symbol space text $> tok

symForParser :: Parser Tok
symForParser = symbol "for" TokFor
