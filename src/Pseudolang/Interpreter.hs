module Pseudolang.Interpreter where

import Pseudolang.Prelude

import Control.Monad.Fail (fail)
import Control.Monad.State (execStateT, get, gets, modify)
import Data.Map.Strict (Map)
import Text.Megaparsec (eof, errorBundlePretty, parse)
import Text.Pretty.Simple (pShow)

import Pseudolang.Parser
import qualified Pseudolang.Lexer as Lexer

type Interpret = StateT (Map Identifier Val) IO

data Val
  = ValBool !Bool
  | ValInt !Integer
  deriving stock (Eq, Ord, Show)

parseAndInterpretToMappingWithInitial :: Text -> Map Identifier Val -> IO (Map Identifier Val)
parseAndInterpretToMappingWithInitial inputText initialMapping = do
  let lexerOutput = parse Lexer.tokenizer "" inputText
  case lexerOutput of
    Left err -> do
      fail $ errorBundlePretty err
    Right tokens -> do
      let initialIndentAmount = 0
          parser = runReaderT astParser initialIndentAmount
          eitherAST = parse (parser <* eof) "" tokens
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
  execStateT (interpretAST ast) initialMapping

interpret :: AST -> IO ()
interpret ast = void $ interpretToMapping ast

interpretAST :: AST -> Interpret ()
interpretAST (AST topLevels) = for_ topLevels interpretTopLevel

interpretTopLevel :: TopLevel -> Interpret ()
interpretTopLevel = \case
  TopLevelFunDef _ -> undefined
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
interpretForLoop (ForLoop assignment@(Assignment identifier _) direction goalExpr bodyStatements) = do
  interpretAssignment assignment
  loopWhileNotGoal
  where
    -- This is the function that we can use to either add one or subtract one
    -- from the identifier that we are looping over.
    identForModifier :: Integer -> Integer
    identForModifier x =
      case direction of
        ForDirectionDownTo -> x - 1
        ForDirectionTo -> x + 1

    loop :: [Statement] -> Interpret ()
    loop [] = do
      -- If there are no more statements left to interpret in the loop body,
      -- then we either increment or decrement the loop identifier, and then loop again.
      i <- getLoopIdentVal
      modify (insertMap identifier (ValInt (identForModifier i)))
      loopWhileNotGoal
    loop (thisStatement : remainingStatements) = do
      interpretStatement thisStatement
      loop remainingStatements

    loopWhileNotGoal :: Interpret ()
    loopWhileNotGoal = do
      i <- getLoopIdentVal
      goal <- interpretExprToInt goalExpr
      if i == goal
        then pure ()
        else loop bodyStatements

    getLoopIdentVal :: Interpret Integer
    getLoopIdentVal = do
      maybeIdentVal <- gets (lookup identifier)
      case maybeIdentVal of
        Nothing -> do
          mapping <- get
          fail $
            "The identifier (" <> show identifier <>
            ") doesn't have an value in the identifier mapping (" <>
            show mapping <>
            ") in a for loop, even though it definitely should"
        Just (ValBool b) -> do
          fail $
            "The identifier (" <> show identifier <>
            ") in a for loop is a boolean and not an integer."
        Just (ValInt i) -> pure i
  

interpretExpr :: Expr -> Interpret Val
interpretExpr = \case
  ExprDivide expr1 expr2 -> do
    int1 <- interpretExprToInt expr1
    int2 <- interpretExprToInt expr2
    pure $ ValInt $ int1 `quot` int2
  ExprGreaterThan expr1 expr2 -> do
    int1 <- interpretExprToInt expr1
    int2 <- interpretExprToInt expr2
    pure $ ValBool $ int1 > int2
  ExprInteger int -> pure $ ValInt int
  ExprLessThan expr1 expr2 -> do
    int1 <- interpretExprToInt expr1
    int2 <- interpretExprToInt expr2
    pure $ ValBool $ int1 < int2
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
