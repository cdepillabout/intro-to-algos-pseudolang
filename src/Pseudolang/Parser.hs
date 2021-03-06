
module Pseudolang.Parser where

import Pseudolang.Prelude hiding (many, some, try)

import Control.Monad.Combinators.Expr (Operator(InfixL, Prefix), makeExprParser)
import Control.Monad.Reader (local)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Set as Set
import Text.Megaparsec (ErrorItem(Label), ParsecT, between, eof, failure, many, sepBy, some, token, try, unPos, (<?>))

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

astParser :: Parser AST
astParser = fmap AST topLevelsParser

topLevelsParser :: Parser [TopLevel]
topLevelsParser = do
  eitherTopLevels <-
    some do
      fmap Left (try $ indentParser *> blankLineParser) <|>
        fmap Right topLevelParser
  pure $ catRights eitherTopLevels

topLevelParser :: Parser TopLevel
topLevelParser = do
  fmap TopLevelFunDef funDefParser <|>
    fmap TopLevelStatement indentedStatementParser <?>
    "top level function or statement"

funDefParser :: Parser FunDef
funDefParser = do
  tokenParser' TokFun
  funcName <- identParser
  tokenParser' TokOpenParen
  funcArgs <- sepBy identParser (tokenParser' TokComma)
  tokenParser' TokCloseParen
  newlineParser
  statements <- indented statementsParser <?>
    "indented statements in function definition"
  pure $ FunDef funcName funcArgs statements

nonEmptyStatementsParser :: Parser (NonEmpty Statement)
nonEmptyStatementsParser = do
  statements <- statementsParser
  case statements of
    [] ->
      failure
        Nothing
        (singletonSet $ Label $ 'E' :| "xpecting at least one statement")
    (h:ts) -> pure (h :| ts)

-- statementsParser :: Parser [Statement]
-- statementsParser = trace "before statementsParser" do
--   traceM "entering statementsParser"
--   -- List of either blank lines or statements
--   eitherStatements <-
--     some do
--       fmap Left (try $ optional indentParser *> blankLineParser) <|>
--         fmap Right indentedStatementParser <?> "statement in a list of statements"
--   traceShowM eitherStatements
--   -- Ignore the blank lines
--   pure $ trace "ending statementsParser" $ catRights eitherStatements
statementsParser :: Parser [Statement]
statementsParser = do
  -- List of either blank lines or statements
  eitherStatements <-
    some do
      fmap Left (try $ optional indentParser *> blankLineParser) <|>
        fmap Right indentedStatementParser <?> "statement in a list of statements"
  -- Ignore the blank lines
  pure $ catRights eitherStatements

-- | Parse any number of blank lines
blankLineParser :: Parser ()
blankLineParser = newlineParser

indentedStatementParser :: Parser Statement
indentedStatementParser = do
  indentParser <?> "indentation beginning a statement"
  statementParser

statementParser :: Parser Statement
statementParser = do
  statementForLoopParser <|>
    statementWhileLoopParser <|>
    statementIfParser <|>
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

statementWhileLoopParser :: Parser Statement
statementWhileLoopParser = do
  whileLoop <- whileParser
  pure $ StatementWhileLoop whileLoop

statementIfParser :: Parser Statement
statementIfParser = do
  if' <- ifParser
  pure $ StatementIf if'

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

elseIfParser :: Parser ElseIf
elseIfParser = do
  indentParser <?> "indentation before an elseif"
  tokenParser' TokElseIf <?> "elseif"
  condition <- exprParser <?> "elseif condition"
  newlineParser <?> "newline after elseif condition line"
  statements <-
    indented nonEmptyStatementsParser <?> "indented statements in an elseif block"
  pure $ ElseIf condition statements

elseParser :: Parser (NonEmpty Statement)
elseParser = do
  indentParser <?> "indentation before an else"
  tokenParser' TokElse
  statementDirectlyAfter <- statementParser <?> "single statement on same line as else"
  maybeStatements <-
    optional (indented statementsParser <?> "indented statements in else block")
  let elseStatements =
        case maybeStatements of
          Nothing -> statementDirectlyAfter :| []
          Just statements -> statementDirectlyAfter :| statements
  pure elseStatements

elseIfElseParser :: Parser ElseIfElse
elseIfElseParser = do
  -- An elseif block is first checked against the correct amount of indentation.
  -- We need this try here incase the indentation matches, but then we actually
  -- find an else block.
  elseIfBlocks <- many $ try elseIfParser
  elseStatements <- elseParser
  pure $ ElseIfElse elseIfBlocks elseStatements

ifParser :: Parser If
ifParser = do
  tokenParser' TokIf
  condition <- exprParser <?> "if condition"
  newlineParser <?> "newline after if condition line"
  statements <-
    indented nonEmptyStatementsParser <?> "indented statements in an if-then block"
  elseIfElse <- try $ optional elseIfElseParser
  pure $ If condition statements elseIfElse

whileParser :: Parser WhileLoop
whileParser = do
  tokenParser' TokWhile
  condition <- exprParser <?> "while loop condition"
  newlineParser <?> "newline after while loop line"
  statements <- indented statementsParser <?> "indented statements in while loop"
  pure $ WhileLoop condition statements

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

stringLiteralParser :: Parser Expr
stringLiteralParser = tokenParser f
  where
    f :: Tok -> Maybe Expr
    f (TokString str) = Just (ExprString str)
    f _ = Nothing

assignmentLHSParser :: Parser AssignmentLHS
assignmentLHSParser = do
  try (fmap AssignmentLHSArrayIndex arrayIndexParser) <|>
    try (fmap AssignmentLHSProperty propertyParser) <|>
    (fmap AssignmentLHSIdentifier identParser) <|>
    (fmap AssignmentLHSTuple assignmentLHSTupleParser)

assignmentLHSTupleParser :: Parser [AssignmentLHS]
assignmentLHSTupleParser = do
  between
    (tokenParser' TokOpenParen)
    (tokenParser' TokCloseParen)
    (sepBy assignmentLHSParser (tokenParser' TokComma))

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

tupleParser :: Parser Tuple
tupleParser = do
  exprs <-
    between
      (tokenParser' TokOpenParen)
      (tokenParser' TokCloseParen)
      (sepBy exprParser (tokenParser' TokComma))
  pure $ Tuple exprs

funCallParser :: Parser FunCall
funCallParser = do
  funName <- identParser
  args <- tupleParser
  pure $ FunCall funName args

propertyParser :: Parser Property
propertyParser = do
  ident <- identParser
  tokenParser' TokPeriod
  propertyIdent <- identParser
  pure $ Property ident propertyIdent

infinityParser :: Parser Expr
infinityParser = do
  tokenParser' TokInfinity
  pure ExprInfinity

parenExprParser :: Parser Expr
parenExprParser = do
  between (tokenParser' TokOpenParen) (tokenParser' TokCloseParen) exprParser

termParser :: Parser Expr
termParser =
  try parenExprParser
  <|> fmap ExprTuple tupleParser
  <|> arrayLiteralParser
  <|> integerParser
  <|> stringLiteralParser
  <|> infinityParser
  <|> try (fmap ExprFunCall funCallParser)
  <|> try (fmap ExprArrayIndex arrayIndexParser)
  <|> try (fmap ExprProperty propertyParser)
  <|> fmap ExprVar identParser
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
  , [ binary (tokenParser' TokDoubleEquals) ExprEquals
    , binary (tokenParser' TokNotEquals) ExprNotEquals
    , binary (tokenParser' TokLessThan) ExprLessThan
    , binary (tokenParser' TokLessThanOrEqualTo) ExprLessThanOrEqualTo
    , binary (tokenParser' TokGreaterThan) ExprGreaterThan
    , binary (tokenParser' TokGreaterThanOrEqualTo) ExprGreaterThanOrEqualTo
    ]
  , [ binary (tokenParser' TokAnd) ExprAnd
    , binary (tokenParser' TokOr) ExprOr
    ]
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
