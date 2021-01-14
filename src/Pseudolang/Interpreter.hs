module Pseudolang.Interpreter where

import Pseudolang.Prelude

import Control.Monad.State (execState, gets, modify)
import Data.Map.Strict (Map)
import Text.Megaparsec (parse, eof)
import Text.Pretty.Simple (pShow)

import Pseudolang.Parser

type Interpret = StateT (Map Identifier Val) IO

data Val
  = ValBool !Bool
  | ValInt !Integer
  deriving stock (Eq, Ord, Show)

parseAndInterpretToMappingWithInitial :: Text -> Map Identifier Val -> IO (Map Identifier Val)
parseAndInterpretToMappingWithInitial text initialMapping = do
  let initialIndentAmount = 0
      parser = runReaderT astParser initialIndentAmount
      eitherAST = parse (parser <* eof) "" text
  case eitherAST of
    Right ast -> interpretToMappingWithInitial ast initialMapping
    Left err -> error $ "Error in parsing: " <> (unpack $ pShow err)

parseAndInterpretToMapping :: Text -> IO (Map Identifier Val)
parseAndInterpretToMapping text =
  parseAndInterpretToMappingWithInitial text mempty

parseAndInterpret :: Text -> IO ()
parseAndInterpret text = void $ parseAndInterpretToMapping text

interpretToMapping :: AST -> IO (Map Identifier Val)
interpretToMapping ast = interpretToMappingWithInitial ast mempty

interpretToMappingWithInitial :: AST -> Map Identifier Val -> IO (Map Identifier Val)
interpretToMappingWithInitial ast initialMapping =
  execState (interpretAST ast) initialMapping

interpret :: AST -> IO ()
interpret ast = void $ interpretToMapping ast

interpretAST :: AST -> Interpret ()
interpretAST (AST topLevels) = for_ topLevels interpretTopLevel

interpretTopLevel :: TopLevel -> Interpret ()
interpretTopLevel = \case
  TopLevelFuncDef _ -> undefined
  TopLevelStatement statement -> interpretStatement statement

interpretStatement :: Statement -> Interpret ()
interpretStatement = \case
  StatementAssignment assignment -> interpretAssignment assignment
  StatementForLoop forLoop -> interpretForLoop forLoop
  StatementIf expr statements elseIf -> undefined
  StatementReturn expr -> undefined

interpretAssignment :: Assignment -> Interpret ()
interpretAssignment (Assignment identifier expr) = do
  val <- interpretExpr expr
  modify (insertMap identifier val)

interpretForLoop :: ForLoop -> Interpret ()
interpretForLoop forLoop = undefined

interpretExpr :: Expr -> Interpret Val
interpretExpr = \case
  ExprDivide expr1 expr2 -> do
    int1 <- interpretExprToInt expr1
    int2 <- interpretExprToInt expr2
    pure $ ValInt $ int1 `quot` int2
  ExprGreaterThan expr1 expr2 -> do
    bool1 <- interpretExprToBool expr1
    bool2 <- interpretExprToBool expr2
    pure $ ValBool $ bool1 > bool2
  ExprInteger int -> pure $ ValInt int
  ExprLessThan expr1 expr2 -> do
    bool1 <- interpretExprToBool expr1
    bool2 <- interpretExprToBool expr2
    pure $ ValBool $ bool1 < bool2
  ExprMinus expr1 expr2 -> do
    int1 <- interpretExprToInt expr1
    int2 <- interpretExprToInt expr2
    pure $ ValInt $ int1 - int2
  ExprNegate expr -> do
    int <- interpretExprToInt expr
    pure $ ValInt $ negate int
  ExprParens expr -> interpretExpr expr
  ExprPlus expr1 expr2 -> do
    int1 <- interpretExprToInt expr1
    int2 <- interpretExprToInt expr2
    pure $ ValInt $ int1 + int2
  ExprTimes expr1 expr2 -> do
    int1 <- interpretExprToInt expr1
    int2 <- interpretExprToInt expr2
    pure $ ValInt $ int1 * int2
  ExprVar identifier -> do
    maybeVal <- gets (lookup identifier)
    case maybeVal of
      Nothing -> error $ "No value for identifier: " <> show identifier
      Just val -> pure val

interpretExprToInt :: Expr -> Interpret Integer
interpretExprToInt expr = do
  val <- interpretExpr expr
  case val of
    ValInt int -> pure int
    val -> error $ "Expecting an int, but got a val: " <> show val

interpretExprToBool :: Expr -> Interpret Bool
interpretExprToBool expr = do
  val <- interpretExpr expr
  case val of
    ValBool bool' -> pure bool'
    val -> error $ "Expecting a bool, but got a val: " <> show val
