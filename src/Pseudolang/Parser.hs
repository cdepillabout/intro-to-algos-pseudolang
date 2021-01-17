
module Pseudolang.Parser where

import Pseudolang.Prelude hiding (many, some, try)

import Control.Monad.Combinators.Expr (Operator(InfixL, Prefix), makeExprParser)
import Control.Monad.Reader (local)
import qualified Data.Set as Set
import Text.Megaparsec (ErrorItem, ParsecT, Pos, SourcePos, between, choice, eof, getOffset, many, mkPos, notFollowedBy, sepBy, some, token, try, unPos, (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, hspace1, letterChar, string)

import Pseudolang.Lexer (Tok(..), Token(Token))

type IndentAmount = Int

type Parser = ReaderT IndentAmount (ParsecT Void [Token] Identity)

newtype AST = AST { unAST :: [TopLevel] }
  deriving stock (Eq, Ord, Show)

data TopLevel = TopLevelStatement Statement | TopLevelFunDef FunDef
  deriving stock (Eq, Ord, Show)

data Statement
  = StatementAssignment Assignment
  | StatementExpr Expr
  | StatementFunCall FunCall
  | StatementForLoop ForLoop
  | StatementIf Expr (NonEmpty Statement) (Maybe ElseIf)
  | StatementReturn Expr
  deriving stock (Eq, Ord, Show)

data ElseIf = ElseIf
  { elseIfs :: [(Expr, NonEmpty Statement)]
  , elseStatements :: NonEmpty Statement
  }
  deriving stock (Eq, Ord, Show)

data FunDef = FunDef Identifier [Identifier] [Statement]
  deriving stock (Eq, Ord, Show)

data FunCall = FunCall Identifier [Expr]
  deriving stock (Eq, Ord, Show)

data Assignment = Assignment AssignmentLHS Expr
  deriving stock (Eq, Ord, Show)

data AssignmentLHS
  = AssignmentLHSIdentifier Identifier
  | AssignmentLHSArrayIndex ArrayIndex
  deriving stock (Eq, Ord, Show)

data ForLoop = ForLoop Assignment ForDirection Expr [Statement]
  deriving stock (Eq, Ord, Show)

data ForDirection = ForDirectionDownTo | ForDirectionTo
  deriving stock (Eq, Ord, Show)

data ArrayIndex = ArrayIndex Identifier Expr
  deriving stock (Eq, Ord, Show)

data Expr
  = ExprArrayIndex ArrayIndex -- ^ This is like @A[3]@.
  | ExprArrayLit [Expr] -- ^ This is like @[1, 3, x]@.
  | ExprDivide Expr Expr
  | ExprFunCall FunCall
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

astParser :: Parser AST
astParser = fmap AST $ many topLevelParser

topLevelParser :: Parser TopLevel
topLevelParser =
  fmap TopLevelFunDef funDefParser <|>
  fmap TopLevelStatement statementParser

funDefParser :: Parser FunDef
funDefParser = do
  tokenParser' TokFun
  funcName <- identParser
  tokenParser' TokOpenParen
  funcArgs <- sepBy identParser (tokenParser' TokComma)
  tokenParser' TokCloseParen
  newlineParser
  statements <- indented statementsParser <?> "indented statements in function def"
  pure $ FunDef funcName funcArgs statements

statementsParser :: Parser [Statement]
statementsParser = do
  some statementParser

statementParser :: Parser Statement
statementParser = do
  indentParser
  statementForLoopParser <|>
    statementReturnParser <|>
    try statementFunCallParser <|>
    statementAssignmentParser <?>
    "statement"

statementAssignmentParser :: Parser Statement
statementAssignmentParser = do
  assignment <- assignmentParser <?> "assignment statement"
  newlineParser <|> eof <?> "newline or EOF after assignment statement"
  pure $ StatementAssignment assignment

statementFunCallParser :: Parser Statement
statementFunCallParser = do
  funCall <- funCallParser
  newlineParser <|> eof <?> "newline or EOF after function call statement"
  pure $ StatementFunCall funCall

statementReturnParser :: Parser Statement
statementReturnParser = do
  tokenParser' TokReturn
  expr <- exprParser <?> "return statement expr"
  newlineParser <|> eof <?> "newline or EOF after return statement"
  pure $ StatementReturn expr

statementForLoopParser :: Parser Statement
statementForLoopParser = do
  forLoop <- forParser
  pure $ StatementForLoop forLoop

getCurrIndent :: Parser IndentAmount
getCurrIndent = ask

indentParser :: Parser ()
indentParser = do
  expectedIndentAmount <- getCurrIndent
  when (expectedIndentAmount > 0) $
    tokenParser (f expectedIndentAmount)
  where
    f :: Int -> Tok -> Maybe ()
    f indentAmount (TokIndent i) | indentAmount == unPos i = Just ()
    f _ _ = Nothing

newlineParser :: Parser ()
newlineParser = tokenParser' TokNewline

forParser :: Parser ForLoop
forParser = do
  tokenParser' TokFor
  assignment <- assignmentParser <?> "for loop assignment"
  forDirection <- forDirectionParser <?> "for loop direction (to / downto)"
  goal <- exprParser <?> "for loop goal expr"
  newlineParser <?> "newline after for loop line"
  statements <- indented statementsParser <?> "indented statements in for loop"
  pure $ ForLoop assignment forDirection goal statements

forDirectionParser :: Parser ForDirection
forDirectionParser =
  (tokenParser' TokDownTo $> ForDirectionDownTo) <|>
  (tokenParser' TokTo $> ForDirectionTo)

indented :: Parser a -> Parser a
indented parser = do
  local (+ 2) parser

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

assignmentLHSParser :: Parser AssignmentLHS
assignmentLHSParser = do
  try (fmap AssignmentLHSArrayIndex arrayIndexParser) <|>
    (fmap AssignmentLHSIdentifier identParser)

assignmentParser :: Parser Assignment
assignmentParser = do
  assignLHS <- assignmentLHSParser
  equalsParser
  expr <- exprParser
  pure $ Assignment assignLHS expr

exprParser :: Parser Expr
exprParser = makeExprParser termParser exprTable <?> "expression"

arrayLiteralParser :: Parser Expr
arrayLiteralParser = do
  exprs <- between (tokenParser' TokOpenSquareBracket) (tokenParser' TokCloseSquareBracket)
    (sepBy exprParser (tokenParser' TokComma))
  pure $ ExprArrayLit exprs

arrayIndexParser :: Parser ArrayIndex
arrayIndexParser = do
  ident <- identParser
  indexExpr <-
    between
      (tokenParser' TokOpenSquareBracket)
      (tokenParser' TokCloseSquareBracket)
      exprParser
  pure $ ArrayIndex ident indexExpr

termParser :: Parser Expr
termParser =
  between (tokenParser' TokOpenParen) (tokenParser' TokCloseParen) exprParser
  <|> arrayLiteralParser
  <|> integerParser
  <|> try (fmap ExprFunCall funCallParser)
  <|> try (fmap ExprArrayIndex arrayIndexParser)
  <|> fmap ExprVar identParser
  <?> "term"

funCallParser :: Parser FunCall
funCallParser = do
  funName <- identParser
  (args :: [Expr]) <-
    between
      (tokenParser' TokOpenParen)
      (tokenParser' TokCloseParen)
      (sepBy exprParser (tokenParser' TokComma))
  pure $ FunCall funName args

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
