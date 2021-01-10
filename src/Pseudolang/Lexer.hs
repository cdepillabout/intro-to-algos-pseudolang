
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
  | TokIdentifier Text -- ^ Identifier name.
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
  choice
    [ reservedSymsParser
    , try reservedAlphaWordsParser
    , identifierParser
    , newlineParser
    , indentParser
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
  [ ("for", TokFor)
  , ("downto", TokDownTo)
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
  [ ("=", TokEquals)
  , ("<", TokLessThan)
  , (")", TokCloseParen)
  , ("}", TokCloseCurlyBrace)
  , ("]", TokCloseSquareBracket)
  , (",", TokComma)
  , (">", TokGreaterThan)
  , ("-", TokMinus)
  , ("(", TokOpenParen)
  , ("{", TokOpenCurlyBrace)
  , ("[", TokOpenSquareBracket)
  , ("+", TokPlus)
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

newlineParser :: Parser Token
newlineParser = tokenParser (char '\n' $> TokNewline)

indentParser :: Parser Token
indentParser = tokenParser do
  spaces <- some (char ' ')
  pure $ TokIndent (mkPos $ length spaces)
