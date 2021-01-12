
module Pseudolang.Lexer where

import Pseudolang.Prelude hiding (many, some, try)

import Text.Megaparsec (ParsecT, Pos, SourcePos, choice, getOffset, many, mkPos, notFollowedBy, some, try)
import Text.Megaparsec.Char (alphaNumChar, char, hspace1, letterChar, string)
import qualified Text.Megaparsec.Char.Lexer as Megaparsec.Lexer

type Parser = ParsecT Void Text Identity

data Token = Token
  { token :: Tok
  , startPos :: Int
  , endPos :: Int
  } deriving (Eq, Ord, Show)

data Tok
  = TokCloseCurlyBrace
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
  | TokPlus
  | TokReturn
  | TokTimes
  | TokTo
  deriving (Eq, Ord, Show)

tokenizer :: Parser [Token]
tokenizer = some lexer

lexer :: Parser Token
lexer =
  choice
    [ reservedSymsParser
    , try reservedAlphaWordsParser
    , identifierParser
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

reservedAlphaWords :: [(Text, Tok)]
reservedAlphaWords =
  [ ("fun", TokFun)
  , ("downto", TokDownTo)
  , ("for", TokFor)
  , ("return", TokReturn)
  , ("to", TokTo)
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
integerParser = tokenParser (fmap TokInteger Megaparsec.Lexer.decimal)

newlineParser :: Parser Token
newlineParser = tokenParser (char '\n' $> TokNewline)

indentParser :: Parser Token
indentParser = tokenParser do
  spaces <- some (char ' ')
  pure $ TokIndent (mkPos $ length spaces)
