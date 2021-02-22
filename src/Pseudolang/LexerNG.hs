
module Pseudolang.LexerNG where

import Pseudolang.Prelude hiding (many, some, try)

import Text.Megaparsec (ParsecT, Pos, choice, eof, getOffset, many, manyTill, mkPos, notFollowedBy, some, try)
import Text.Megaparsec.Char (alphaNumChar, char, hspace1, letterChar, string, symbolChar)
import qualified Text.Megaparsec.Char.Lexer as Megaparsec.Lexer

type IndentAmount = Int
type Parser = ReaderT IndentAmount (ParsecT Void Text Identity)

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
  | TokDoubleEquals -- ^ @==@
  | TokDownTo
  | TokElse
  | TokElseIf
  | TokEquals -- ^ @=@
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
  | TokNotEquals -- ^ @/=@
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

-- tokenizer :: Parser [Token]
-- tokenizer = tokenizer' <* eof

-- tokenizer' :: Parser [Token]
-- tokenizer' = some lexer

-- lexer :: Parser Token
-- lexer = do
--   -- Throw away a comment at the start of a line.  The indentParser function
--   -- handles comments that have been indented.  The newlineParser handles
--   -- comments that happen after a newline.
--   void $ optional lineCommentParser
--   choice
--     [ reservedSymsParser
--     , reservedAlphaWordsParser
--     , identifierParser
--     , stringLiteralParser
--     , newlineParser
--     , indentParser
--     , integerParser
--     ]

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

lexemeParser' :: Parser a -> Parser a
lexemeParser' p = p <* spaceParser

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
  , ("/=", TokNotEquals)
  , ("<", TokLessThan)
  , ("<=", TokLessThanOrEqualTo)
  , ("=", TokEquals)
  , ("==", TokDoubleEquals)
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

openParenParser :: Parser ()
openParenParser = lexemeParser' $ void $ string "("

closeParenParser :: Parser ()
closeParenParser = lexemeParser' $ void $ string ")"

timesParser :: Parser ()
timesParser = lexemeParser' $ void $ string "*"

plusParser :: Parser ()
plusParser = lexemeParser' $ void $ string "+"

commaParser :: Parser ()
commaParser = lexemeParser' $ void $ string ","

minusParser :: Parser ()
minusParser = lexemeParser' $ void $ string "-"

periodParser :: Parser ()
periodParser = lexemeParser' $ void $ string "."

divideParser :: Parser ()
divideParser = lexemeParser' $ void $ string "/"

notEqualsParser :: Parser ()
notEqualsParser = lexemeParser' $ void $ string "/="

lessThanParser :: Parser ()
lessThanParser = lexemeParser' $ void $ string "<"

lessThanOrEqualToParser :: Parser ()
lessThanOrEqualToParser = lexemeParser' $ void $ string "<="

equalsParser :: Parser ()
equalsParser = lexemeParser' $ void $ string "="

doubleEqualsParser :: Parser ()
doubleEqualsParser = lexemeParser' $ void $ string "=="

greaterThanParser :: Parser ()
greaterThanParser = lexemeParser' $ void $ string ">"

greaterThanOrEqualToParser :: Parser ()
greaterThanOrEqualToParser = lexemeParser' $ void $ string ">="

openSquareBracketParser :: Parser ()
openSquareBracketParser = lexemeParser' $ void $ string "["

closeSquareBracketParser :: Parser ()
closeSquareBracketParser = lexemeParser' $ void $ string "]"

openCurlyBraceParser :: Parser ()
openCurlyBraceParser = lexemeParser' $ void $ string "{"

closeCurlyBraceParser :: Parser ()
closeCurlyBraceParser = lexemeParser' $ void $ string "}"

infinityParser :: Parser ()
infinityParser = lexemeParser' $ void $ string "∞"

identifierParser :: Parser Text
identifierParser = do
  lexemeParser' do
    firstChar <- letterChar
    (remainingChars :: [Char]) <- many (alphaNumChar <|> char '-')
    let ident = pack (firstChar : remainingChars)
    pure ident

integerParser :: Parser Integer
integerParser = lexemeParser' Megaparsec.Lexer.decimal

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
