
module Pseudolang.Parser where

import Pseudolang.Prelude hiding (many, some, try)

import qualified Data.Set as Set
import Control.Monad.Combinators.Expr (Operator(InfixL, Prefix), makeExprParser)
import Text.Megaparsec (ErrorItem, ParsecT, Pos, SourcePos, between, choice, getOffset, many, mkPos, notFollowedBy, some, token, try, (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, hspace1, letterChar, string)

import Pseudolang.Lexer (Tok(..), Token(Token))

type Parser = ParsecT Void [Token] Identity

newtype AST = AST { unAST :: TopLevelList }
  deriving stock (Eq, Ord, Show)

newtype TopLevelList = TopLevelList
  { unTopLevelList :: NonEmpty (Either Statement FuncDef)
  }
  deriving stock (Eq, Ord, Show)

data TopLevel = TopLevelStatement Statement | TopLevelFuncDef FuncDef
  deriving stock (Eq, Ord, Show)

data Statement
  = StatementAssignment Assignment
  | StatementForLoop Assignment ForDirection Expr [Statement]
  | StatementIf Expr [Statement]
  | StatementReturn Expr
  deriving stock (Eq, Ord, Show)

data FuncDef = FuncDef Identifier [Identifier] [Statement]
  deriving stock (Eq, Ord, Show)

data Assignment = Assignment Identifier Expr
  deriving stock (Eq, Ord, Show)

data ForDirection = DownTo | To
  deriving stock (Eq, Ord, Show)

data Expr
  = ExprDivide Expr Expr
  | ExprGreaterThan Expr Expr
  | ExprInteger Integer
  | ExprLessThan Expr Expr
  | ExprMinus Expr Expr
  | ExprNegate Expr
  | ExprParens Expr
  | ExprPlus Expr Expr
  | ExprTimes Expr Expr
  | ExprVar Identifier
  deriving stock (Eq, Ord, Show)

newtype Identifier = Identifier Text
  deriving stock (Eq, Ord, Show)

statementsParser :: Parser [Statement]
statementsParser = do
  some statementParser

statementParser :: Parser Statement
statementParser = do
  indentAmount <- getCurrIdent
  tokenParser f
  forParser <|> assignmentParser
  where
    f :: Int -> Tok -> Maybe ()
    f indentAmount (TokIndent i) | indentAmount == i = Just ()
    f _ _ = Nothing

forParser :: Parser Statement
forParser = do
  tokenParser' TokFor
  assignment <- assignmentParser
  forDirection <- forDirectionParser
  goal <- exprParser
  void (tokenParser' TokNewline)
  indented statementsParser

-- TODO: Figure out how to work with indents.  Do I need a StateT around my
-- ParsecT, or the other way around?
indented :: Parser a -> Parser a
indented = undefined

identParser :: Parser Identifier
identParser = tokenParser f
  where
    f :: Tok -> Maybe Identifier
    f (TokIdentifier ident) = Just (Identifier ident)
    f _ = Nothing

tokenParser :: forall a. (Tok -> Maybe a) -> Parser a
tokenParser f = token go Set.empty
  where
    go :: Token -> Maybe a
    go (Token t _ _) = f t

tokenParser' :: Tok -> Parser ()
tokenParser' tok = tokenParser f
  where
    f :: Tok -> Maybe ()
    f t = if t == tok then Just () else Nothing

equalsParser :: Parser ()
equalsParser = tokenParser' TokEquals

integerParser :: Parser Expr
integerParser = tokenParser f
  where
    f :: Tok -> Maybe Expr
    f (TokInteger i) = Just (ExprInteger i)
    f _ = Nothing

assignmentParser :: Parser Assignment
assignmentParser = do
  ident <- identParser
  equalsParser
  expr <- exprParser
  pure $ Assignment ident expr

exprParser :: Parser Expr
exprParser = makeExprParser termParser exprTable <?> "expression"

termParser :: Parser Expr
termParser =
  between (tokenParser' TokOpenParen) (tokenParser' TokCloseParen) exprParser
  <|> integerParser
  <|> (fmap ExprVar identParser)
  <?> "term"

exprTable :: [[Operator Parser Expr]]
exprTable =
  [ [ prefix (tokenParser' TokMinus) ExprNegate
    ]
  , [ binary (tokenParser' TokTimes) ExprTimes
    , binary (tokenParser' TokDivide) ExprDivide
    ]
  , [ binary (tokenParser' TokPlus) ExprPlus
    , binary (tokenParser' TokMinus) ExprMinus
    ]
  , [ binary (tokenParser' TokLessThan) ExprLessThan
    , binary (tokenParser' TokGreaterThan) ExprGreaterThan
    ]
  ]
  where
    binary :: Parser () -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary parser exprCreator = InfixL (parser $> exprCreator)

    prefix :: Parser () -> (Expr -> Expr) -> Operator Parser Expr
    prefix parser exprCreator = Prefix (parser $> exprCreator)
