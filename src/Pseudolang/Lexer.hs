
module Pseudolang.Lexer where

import Pseudolang.Prelude hiding (many, some, try)

import Text.Megaparsec (ParsecT, Pos, SourcePos, choice, eof, getOffset, many, manyTill, mkPos, notFollowedBy, some, try)
import Text.Megaparsec.Char (alphaNumChar, char, hspace1, letterChar, string)
import qualified Text.Megaparsec.Char.Lexer as Megaparsec.Lexer

type Parser = ParsecT Void Text Identity

data Token = Token
  { token :: Tok
  , startPos :: Int
  , endPos :: Int
  } deriving (Eq, Ord, Show)

data Tok
  = TokAnd
  | TokCloseCurlyBrace
  | TokCloseParen
  | TokCloseSquareBracket
  | TokComma
  | TokDivide
  | TokDownTo
  | TokEquals
  | TokFor
  | TokFun
  | TokGreaterThan
  | TokIdentifier Text -- ^ Identifier name.
  | TokIndent Pos -- ^ How many spaces this indent includes.
  | TokInteger Integer
  | TokLessThan
  | TokMinus
  | TokNewline
  | TokOpenCurlyBrace
  | TokOpenParen
  | TokOpenSquareBracket
  | TokOr
  | TokPeriod
  | TokPlus
  | TokReturn
  | TokString Text
  | TokTimes
  | TokTo
  | TokWhile
  deriving (Eq, Ord, Show)

tokenizer :: Parser [Token]
tokenizer = tokenizer' <* eof

tokenizer' :: Parser [Token]
tokenizer' = some lexer

lexer :: Parser Token
lexer =
  choice
    [ reservedSymsParser
    , try reservedAlphaWordsParser
    , identifierParser
    , stringLiteralParser
    , newlineParser
    , indentParser
    , integerParser
    ]

spaceParser :: Parser ()
spaceParser = Megaparsec.Lexer.space hspace1 empty empty

tokenParser :: Parser Tok -> Parser Token
tokenParser p = do
  startingOffset <- getOffset
  tok <- p
  endingOffset <- getOffset
  pure $ Token tok startingOffset endingOffset

lexemeParser :: Parser Tok -> Parser Token
lexemeParser p = tokenParser p <* spaceParser

stringLiteralParser :: Parser Token
stringLiteralParser = do
  lexemeParser do
    void $ char '"'
    str <- manyTill Megaparsec.Lexer.charLiteral (char '"')
    pure $ TokString (pack str)

reservedAlphaWords :: [(Text, Tok)]
reservedAlphaWords =
  [ ("and", TokAnd)
  , ("downto", TokDownTo)
  , ("for", TokFor)
  , ("fun", TokFun)
  , ("or", TokOr)
  , ("return", TokReturn)
  , ("to", TokTo)
  , ("while", TokWhile)
  ]

reservedAlphaWordsParser :: Parser Token
reservedAlphaWordsParser = do
  lexemeParser do
    tok <- choice $ fmap (\(str, tok) -> string str $> tok) reservedAlphaWords
    notFollowedBy alphaNumChar
    pure tok

reservedSyms :: [(Text, Tok)]
reservedSyms =
  [ ("(", TokOpenParen)
  , (")", TokCloseParen)
  , ("*", TokTimes)
  , ("+", TokPlus)
  , (",", TokComma)
  , ("-", TokMinus)
  , (".", TokPeriod)
  , ("/", TokDivide)
  , ("<", TokLessThan)
  , ("=", TokEquals)
  , (">", TokGreaterThan)
  , ("[", TokOpenSquareBracket)
  , ("]", TokCloseSquareBracket)
  , ("{", TokOpenCurlyBrace)
  , ("}", TokCloseCurlyBrace)
  ]

reservedSymsParser :: Parser Token
reservedSymsParser = do
  lexemeParser (choice $ fmap (\(str, tok) -> string str $> tok) reservedSyms)

identifierParser :: Parser Token
identifierParser = do
  lexemeParser do
    firstChar <- letterChar
    (remainingChars :: [Char]) <- many (alphaNumChar <|> char '-')
    let ident = pack (firstChar : remainingChars)
    pure $ TokIdentifier ident

integerParser :: Parser Token
integerParser = lexemeParser (fmap TokInteger Megaparsec.Lexer.decimal)

newlineParser :: Parser Token
newlineParser = tokenParser (char '\n' $> TokNewline)

indentParser :: Parser Token
indentParser = tokenParser do
  spaces <- some (char ' ')
  pure $ TokIndent (mkPos $ length spaces)
