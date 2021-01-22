
module Pseudolang.Lexer where

import Pseudolang.Prelude hiding (many, some, try)

import Text.Megaparsec (ParsecT, Pos, SourcePos, choice, eof, getOffset, many, manyTill, mkPos, notFollowedBy, some, try)
import Text.Megaparsec.Char (alphaNumChar, char, hspace1, letterChar, string, symbolChar)
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
  | TokElse
  | TokElseIf
  | TokEquals
  | TokFor
  | TokFun
  | TokGreaterThan
  | TokGreaterThanOrEqualTo
  | TokIdentifier Text -- ^ Identifier name.
  | TokIf
  | TokIndent Pos -- ^ How many spaces this indent includes.
  | TokInfinity
  | TokInteger Integer
  | TokLessThan
  | TokLessThanOrEqualTo
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
lexer = do
  -- Throw away a comment at the start of a line.  The indentParser function
  -- handles comments that have been indented.  The newlineParser handles
  -- comments that happen after a newline.
  void $ optional lineCommentParser
  choice
    [ reservedSymsParser
    , reservedAlphaWordsParser
    , identifierParser
    , stringLiteralParser
    , newlineParser
    , indentParser
    , integerParser
    ]

lineCommentParser :: Parser ()
lineCommentParser = Megaparsec.Lexer.skipLineComment "//"

spaceParser :: Parser ()
spaceParser =
  Megaparsec.Lexer.space
    hspace1
    lineCommentParser
    empty

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
  , ("else", TokElse)
  , ("elseif", TokElseIf)
  , ("for", TokFor)
  , ("fun", TokFun)
  , ("if", TokIf)
  , ("infinity", TokInfinity)
  , ("or", TokOr)
  , ("return", TokReturn)
  , ("to", TokTo)
  , ("while", TokWhile)
  ]

reservedAlphaWordParser :: Text -> Tok -> Parser Tok
reservedAlphaWordParser str tok = do
  void $ string str
  notFollowedBy alphaNumChar
  pure tok

-- | Parse a reserved alphaword (like @if@ or @and@).
--
-- Backtracks on failure.  This can be used immediately before the identifier
-- parser without any trouble.
reservedAlphaWordsParser :: Parser Token
reservedAlphaWordsParser = do
  lexemeParser $ choice reservedAlphaWordParsers
  where
    reservedAlphaWordParsers :: [Parser Tok]
    reservedAlphaWordParsers =
      fmap (\(str, tok) -> try $ reservedAlphaWordParser str tok) reservedAlphaWords

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
  , ("<=", TokLessThanOrEqualTo)
  , ("=", TokEquals)
  , (">", TokGreaterThan)
  , (">=", TokGreaterThanOrEqualTo)
  , ("[", TokOpenSquareBracket)
  , ("]", TokCloseSquareBracket)
  , ("{", TokOpenCurlyBrace)
  , ("}", TokCloseCurlyBrace)
  , ("∞", TokInfinity)
  ]

reservedSymParser :: Text -> Tok -> Parser Tok
reservedSymParser str tok = do
  void $ string str
  notFollowedBy symbolChar
  pure tok

-- | Parse a reserved symbol (like @>=@ or @]@).
--
-- Backtracks on failure.  This can be used immediately before the identifier
-- parser without any trouble.
reservedSymsParser :: Parser Token
reservedSymsParser = do
  lexemeParser $ choice reservedSymParsers
  where
    reservedSymParsers :: [Parser Tok]
    reservedSymParsers =
        fmap (\(str, tok) -> try $ reservedSymParser str tok) reservedSyms

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
newlineParser = do
  newlineToken <- tokenParser do
    void $ char '\n'
    pure TokNewline
  -- Throw away a comment at the start of a line.  The indentParser function
  -- handles comments that have been indented.
  void $ optional lineCommentParser
  pure newlineToken

indentParser :: Parser Token
indentParser =
  -- We use lexemeParser here because we specificially need to handle parsing
  -- line comments that come directly after an indent.
  lexemeParser do
    spaces <- some (char ' ')
    pure $ TokIndent (mkPos $ length spaces)
