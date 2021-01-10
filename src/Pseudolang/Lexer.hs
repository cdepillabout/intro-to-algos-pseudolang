
module Pseudolang.Lexer where

import Pseudolang.Prelude hiding (many, some, try)

import Text.Megaparsec (ParsecT, Pos, SourcePos, choice, getOffset, many, notFollowedBy, some, try)
import Text.Megaparsec.Char (alphaNumChar, asciiChar, char, hspace1, string)
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

lexeme :: Parser Tok -> Parser Token
lexeme p = do
  startingOffset <- getOffset
  tok <- p
  endingOffset <- getOffset
  space
  pure $ Token tok startingOffset endingOffset

reservedAlphaWords :: [(Text, Tok)]
reservedAlphaWords =
  [ ("for", TokFor)
  , ("downto", TokDownTo)
  , ("return", TokReturn)
  , ("to", TokTo)
  ]

reservedAlphaWordsParser :: Parser Token
reservedAlphaWordsParser = do
  lexeme do
    tok <- choice $ fmap (\(str, tok) -> string str $> tok) reservedAlphaWords
    notFollowedBy alphaNumChar
    pure tok

reservedSyms :: [(Text, Tok)]
reservedSyms =
  [ ("=", TokEquals)
  , ("<", TokLessThan)
  , (")", TokCloseParen)
  , ("}", TokCloseCurlyBrace)
  , ("]", TokCloseSquareBracket)
  , (",", TokComma)
  , (">", TokGreaterThan)
  , ("-", TokMinus)
  , ("\n", TokNewline)
  , ("(", TokOpenParen)
  , ("{", TokOpenCurlyBrace)
  , ("[", TokOpenSquareBracket)
  , ("+", TokPlus)
  ]

reservedSymsParser :: Parser Token
reservedSymsParser = do
  lexeme (choice $ fmap (\(str, tok) -> string str $> tok) reservedSyms)

identifierParser :: Parser Token
identifierParser = do
  lexeme do
    firstChar <- asciiChar
    (remainingChars :: [Char]) <- many (alphaNumChar <|> char '-')
    let ident = pack (firstChar : remainingChars)
    pure $ TokIdentifier ident
