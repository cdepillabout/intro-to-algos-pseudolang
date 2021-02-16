
module Pseudolang.ParserNG where

import Pseudolang.Prelude hiding (many, some, try)

import Control.Monad.Combinators.Expr (Operator(InfixL, Prefix), makeExprParser)
import Control.Monad.Reader (local)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Set as Set
import Text.Megaparsec (ErrorItem(Label), ParsecT, between, eof, failure, many, sepBy, some, token, try, unPos, (<?>))
import qualified Text.Megaparsec.Debug as Megaparsec.Debug

import Pseudolang.LexerNG (Parser, Tok(..), Token(Token), identifierParser)

newtype AST = AST { unAST :: [TopLevel] }
  deriving stock (Eq, Ord, Show)

data TopLevel = TopLevelStatement Statement | TopLevelFunDef FunDef
  deriving stock (Eq, Ord, Show)

data Statement
  = StatementAssignment Assignment
  | StatementExpr Expr
  | StatementFunCall FunCall
  | StatementForLoop ForLoop
  | StatementIf If
  | StatementReturn Expr
  | StatementWhileLoop WhileLoop
  deriving stock (Eq, Ord, Show)

data If = If Expr (NonEmpty Statement) (Maybe ElseIfElse)
  deriving stock (Eq, Ord, Show)

data ElseIf = ElseIf Expr (NonEmpty Statement)
  deriving stock (Eq, Ord, Show)

data ElseIfElse = ElseIfElse
  { elseIfs :: [ElseIf]
  , elseStatements :: NonEmpty Statement
  }
  deriving stock (Eq, Ord, Show)

data FunDef = FunDef Identifier [Identifier] [Statement]
  deriving stock (Eq, Ord, Show)

data FunCall = FunCall Identifier Tuple
  deriving stock (Eq, Ord, Show)

data Tuple = Tuple [Expr]
  deriving stock (Eq, Ord, Show)

data Property = Property Identifier Identifier
  deriving stock (Eq, Ord, Show)

data Assignment = Assignment AssignmentLHS Expr
  deriving stock (Eq, Ord, Show)

data AssignmentLHS
  = AssignmentLHSIdentifier Identifier
  | AssignmentLHSProperty Property
  | AssignmentLHSArrayIndex ArrayIndex
  | AssignmentLHSTuple [AssignmentLHS]
  deriving stock (Eq, Ord, Show)

data ForLoop = ForLoop Assignment ForDirection Expr [Statement]
  deriving stock (Eq, Ord, Show)

data ForDirection = ForDirectionDownTo | ForDirectionTo
  deriving stock (Eq, Ord, Show)

data WhileLoop = WhileLoop Expr [Statement]
  deriving stock (Eq, Ord, Show)

data ArrayIndex = ArrayIndex Identifier Expr
  deriving stock (Eq, Ord, Show)

data Expr
  = ExprAnd Expr Expr -- ^ This is like @x and y@.
  | ExprArrayIndex ArrayIndex -- ^ This is like @A[3]@.
  | ExprArrayLit [Expr] -- ^ This is like @[1, 3, x]@.
  | ExprDivide Expr Expr
  | ExprEquals Expr Expr  -- ^ This is like @x == y@.
  | ExprFunCall FunCall -- ^ This is like @hello(1, 3)@.
  | ExprGreaterThan Expr Expr
  | ExprGreaterThanOrEqualTo Expr Expr
  | ExprInfinity
  | ExprInteger Integer
  | ExprLessThan Expr Expr
  | ExprLessThanOrEqualTo Expr Expr
  | ExprMinus Expr Expr
  | ExprNegate Expr
  | ExprNotEquals Expr Expr -- ^ This is like @x /= y@.
  | ExprOr Expr Expr -- ^ This is like @x or y@.
  | ExprParens Expr
  | ExprPlus Expr Expr
  | ExprProperty Property -- ^ This is like @A.length@.
  | ExprString Text -- ^ This is like @"hello bye"@.
  | ExprTimes Expr Expr
  | ExprTuple Tuple
  | ExprVar Identifier
  deriving stock (Eq, Ord, Show)

instance Num Expr where
  (+) = ExprPlus
  (-) = ExprMinus
  (*) = ExprTimes
  abs = error "Num Expr, abs not defined"
  signum = error "Num Expr, signum not defined"
  fromInteger = ExprInteger

newtype Identifier = Identifier { unIdentifier :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

dbg :: Show a => String -> Parser a -> Parser a
dbg lbl p = ReaderT $ \indentAmount -> Megaparsec.Debug.dbg lbl (runReaderT p indentAmount)

identParser :: Parser Identifier
identParser = fmap Identifier identifierParser <?> "identifier"
