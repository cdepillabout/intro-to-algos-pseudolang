
module Pseudolang.ParserNG where

import Pseudolang.Prelude hiding (many, some, try)

import Control.Monad.Combinators.Expr (Operator(InfixL, Prefix), makeExprParser)
import Control.Monad.Reader (local)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Set as Set
import Text.Megaparsec (ErrorItem(Label), ParsecT, between, eof, failure, many, sepBy, some, token, try, unPos, (<?>))
import qualified Text.Megaparsec.Debug as Megaparsec.Debug

import Pseudolang.LexerNG

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
  negate = ExprNegate
  abs = error "Num Expr, abs not defined"
  signum = error "Num Expr, signum not defined"
  fromInteger = ExprInteger

newtype Identifier = Identifier { unIdentifier :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

dbg :: Show a => String -> Parser a -> Parser a
dbg lbl p = ReaderT $ \indentAmount -> Megaparsec.Debug.dbg lbl (runReaderT p indentAmount)

-- tokenParser :: forall a. (Tok -> Maybe a) -> Parser a
-- tokenParser f = token go Set.empty
--   where
--     go :: Token -> Maybe a
--     go (Token t _ _) = f t

-- tokenParser' :: Tok -> Parser ()
-- tokenParser' tok = tokenParser f
--   where
--     f :: Tok -> Maybe ()
--     f t = if t == tok then Just () else Nothing

assignmentLHSParser :: Parser AssignmentLHS
assignmentLHSParser =
  -- try (fmap AssignmentLHSArrayIndex arrayIndexParser) <|>
    -- try (fmap AssignmentLHSProperty propertyParser) <|>
  fmap AssignmentLHSIdentifier identParser <|>
  fmap AssignmentLHSTuple assignmentLHSTupleParser

assignmentLHSTupleParser :: Parser [AssignmentLHS]
assignmentLHSTupleParser = do
  between
    openParenParser
    closeParenParser
    (sepBy assignmentLHSParser commaParser)

assignmentParser :: Parser Assignment
assignmentParser = do
  assignLHS <- assignmentLHSParser
  equalsParser
  expr <- exprParser
  pure $ Assignment assignLHS expr

identParser :: Parser Identifier
identParser = fmap Identifier identifierParser <?> "identifier"

exprParser :: Parser Expr
exprParser = makeExprParser termParser exprTable <?> "expression"

-- arrayLiteralParser :: Parser Expr
-- arrayLiteralParser = do
--   exprs <- between (tokenParser' TokOpenSquareBracket) (tokenParser' TokCloseSquareBracket)
--     (sepBy exprParser (tokenParser' TokComma))
--   pure $ ExprArrayLit exprs

-- arrayIndexParser :: Parser ArrayIndex
-- arrayIndexParser = do
--   ident <- identParser
--   indexExpr <-
--     between
--       (tokenParser' TokOpenSquareBracket)
--       (tokenParser' TokCloseSquareBracket)
--       exprParser
--   pure $ ArrayIndex ident indexExpr

-- tupleParser :: Parser Tuple
-- tupleParser = do
--   exprs <-
--     between
--       (tokenParser' TokOpenParen)
--       (tokenParser' TokCloseParen)
--       (sepBy exprParser (tokenParser' TokComma))
--   pure $ Tuple exprs

-- funCallParser :: Parser FunCall
-- funCallParser = do
--   funName <- identParser
--   args <- tupleParser
--   pure $ FunCall funName args

-- propertyParser :: Parser Property
-- propertyParser = do
--   ident <- identParser
--   tokenParser' TokPeriod
--   propertyIdent <- identParser
--   pure $ Property ident propertyIdent

-- infinityParser :: Parser Expr
-- infinityParser = do
--   tokenParser' TokInfinity
--   pure ExprInfinity

parenExprParser :: Parser Expr
parenExprParser = do
  ExprParens <$> between openParenParser closeParenParser exprParser

termParser :: Parser Expr
termParser =
  try parenExprParser
  -- <|> fmap ExprTuple tupleParser
  -- <|> arrayLiteralParser
  <|> fmap ExprInteger integerParser
  -- <|> stringLiteralParser
  -- <|> infinityParser
  -- <|> try (fmap ExprFunCall funCallParser)
  -- <|> try (fmap ExprArrayIndex arrayIndexParser)
  -- <|> try (fmap ExprProperty propertyParser)
  <|> fmap ExprVar identParser
  <?> "term"

exprTable :: [[Operator Parser Expr]]
exprTable =
  [ [ prefix minusParser ExprNegate
    ]
  , [ binary timesParser ExprTimes
    , binary divideParser ExprDivide
    ]
  , [ binary plusParser ExprPlus
    , binary minusParser ExprMinus
    ]
  , [ binary doubleEqualsParser ExprEquals
    , binary notEqualsParser ExprNotEquals
    , binary lessThanParser ExprLessThan
    , binary lessThanOrEqualToParser ExprLessThanOrEqualTo
    , binary greaterThanParser ExprGreaterThan
    , binary greaterThanOrEqualToParser ExprGreaterThanOrEqualTo
    ]
  -- , [ binary andParser ExprAnd
  --   , binary orParser ExprOr
  --   ]
  ]
  where
    binary :: Parser () -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary parser exprCreator = InfixL (parser $> exprCreator)

    prefix :: Parser () -> (Expr -> Expr) -> Operator Parser Expr
    prefix parser exprCreator = Prefix (parser $> exprCreator)

catRights :: [Either x a] -> [a]
catRights = foldr f []
  where
    f :: Either x a -> [a] -> [a]
    f (Right a) accum = a : accum
    f (Left _) accum = accum
