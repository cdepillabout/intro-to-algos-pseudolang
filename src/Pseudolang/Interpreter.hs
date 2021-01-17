module Pseudolang.Interpreter where

import Pseudolang.Prelude

import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Fail (fail)
import Control.Monad.State (execStateT, get, gets, modify, put)
import Data.Map.Strict (Map)
import qualified Data.Vector as Vec
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MVec
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (eof, errorBundlePretty, parse)
import Text.Pretty.Simple (pShow)

import Pseudolang.Parser
import qualified Pseudolang.Lexer as Lexer

type Interpret = ExceptT Val (StateT InterpState IO)

data InterpState = InterpState
  { interpStateVars :: Map Identifier Val
  , interpStateFuns :: Map Identifier FunDef
  }
  -- deriving stock (Eq, Ord, Show)

initialInterpState :: InterpState
initialInterpState = InterpState mempty mempty

addFunDef :: MonadState InterpState m => Identifier -> FunDef -> m ()
addFunDef ident funDef =
  modify
    (\interpState ->
       interpState
       { interpStateFuns = insertMap ident funDef (interpStateFuns interpState)
       }
    )

getFunDef :: MonadState InterpState m => Identifier -> m (Maybe FunDef)
getFunDef ident =
  gets (\interpState -> lookup ident (interpStateFuns interpState))

getVar :: MonadState InterpState m => Identifier -> m (Maybe Val)
getVar ident =
  gets (\interpState -> lookup ident (interpStateVars interpState))

getVarMappings :: MonadState InterpState m => m (Map Identifier Val)
getVarMappings = gets interpStateVars

setVar :: MonadState InterpState m => Identifier -> Val -> m ()
setVar ident val = do
  modify
    (\interpState ->
       interpState
       { interpStateVars = insertMap ident val (interpStateVars interpState)
       }
    )

data Val
  = ValBool !Bool
  | ValInt !Integer
  | ValUnit
  | ValVector (IOVector Val)
  -- deriving stock (Eq, Ord, Show)

instance Show Val where
  show (ValBool b) = show b
  show (ValInt i) = show i
  show ValUnit = show "unit"
  show (ValVector v) = show g
    where
      g :: Vec.Vector Val
      g = unsafePerformIO $ Vec.freeze v
      {-# NOINLINE g #-}

instance Eq Val where
  ValBool b1 == ValBool b2 = b1 == b2
  ValInt i1 == ValInt i2 = i1 == i2
  ValUnit == ValUnit = True
  ValVector v1 == ValVector v2 =
    let v1' = g
        v2' = h
    in v1' == v2'
    where
      g :: Vec.Vector Val
      g = unsafePerformIO $ Vec.freeze v1
      {-# NOINLINE g #-}

      h :: Vec.Vector Val
      h = unsafePerformIO $ Vec.freeze v2
      {-# NOINLINE h #-}

valType :: Val -> String
valType ValBool{} = "bool"
valType ValInt{} = "int"
valType ValUnit{} = "unit"
valType ValVector{} = "array"

-- instance Eq (IOVector Val) where

parseAndInterpretToInterpStateWithInitial ::
     Text -> InterpState -> IO InterpState
parseAndInterpretToInterpStateWithInitial inputText initInterpState = do
  let lexerOutput = parse Lexer.tokenizer "" inputText
  case lexerOutput of
    Left err -> do
      fail $ errorBundlePretty err
    Right tokens -> do
      let initialIndentAmount = 0
          parser = runReaderT astParser initialIndentAmount
          eitherAST = parse (parser <* eof) "" tokens
      case eitherAST of
        Right ast -> interpretToInterpStateWithInitial ast initInterpState
        Left err -> error $ "Error in parsing: " <> (unpack $ pShow err)

parseAndInterpretToInterpState :: Text -> IO InterpState
parseAndInterpretToInterpState text =
  parseAndInterpretToInterpStateWithInitial text initialInterpState

parseAndInterpret :: Text -> IO ()
parseAndInterpret text = void $ parseAndInterpretToInterpState text

interpretToInterpState :: AST -> IO InterpState
interpretToInterpState ast = interpretToInterpStateWithInitial ast initialInterpState

interpretToInterpStateWithInitial :: AST -> InterpState -> IO InterpState
interpretToInterpStateWithInitial ast initInterpState = do
  execStateT (runExceptT $ interpretAST ast) initInterpState

interpret :: AST -> IO ()
interpret ast = void $ interpretToInterpState ast

interpretAST :: AST -> Interpret ()
interpretAST (AST topLevels) = for_ topLevels interpretTopLevel

interpretTopLevel :: TopLevel -> Interpret ()
interpretTopLevel = \case
  TopLevelFunDef funDef -> interpretFunDef funDef
  TopLevelStatement statement -> interpretStatement statement

interpretStatements :: [Statement] -> Interpret ()
interpretStatements statements = for_ statements interpretStatement

interpretFunDef :: FunDef -> Interpret ()
interpretFunDef funDef@(FunDef funName _ _) =
  addFunDef funName funDef

interpretStatement :: Statement -> Interpret ()
interpretStatement = \case
  StatementAssignment assignment -> interpretAssignment assignment
  StatementForLoop forLoop -> interpretForLoop forLoop
  StatementFunCall funCall -> void $ interpretFunCall funCall
  StatementIf expr statements elseIf -> undefined
  StatementReturn expr -> do
    val <- interpretExpr expr
    throwError val

interpretFunCall :: FunCall -> Interpret Val
interpretFunCall (FunCall funName funCallArgs) = do
  maybeFunName <- getFunDef funName
  case maybeFunName of
    Nothing ->
      fail $
        "Trying to call the function (" <> show funName <>
        "), but it doesn't have an value in the known function mappings."
    Just (FunDef _ funDefArgs funDefStatements)
      | length funDefArgs /= length funCallArgs -> do
          fail $
            "Trying to call the function (" <> show funName <>
            "), but it is defined with " <> show (length funDefArgs) <>
            " args, but called with " <> show (length funCallArgs) <> " args"
      | otherwise -> do
          funCallVals <- traverse interpretExpr funCallArgs
          let initialValMapping :: Map Identifier Val
              initialValMapping =
                foldMap
                  (\(funDefArg, funCallVal) -> singletonMap funDefArg funCallVal)
                  (zip funDefArgs funCallVals)
          currInterpState <- get
          put (currInterpState { interpStateVars = initialValMapping })
          returnVal <-
            catchError
              (interpretStatements funDefStatements *> pure ValUnit)
              pure
          put currInterpState
          pure returnVal

interpretAssignment :: Assignment -> Interpret ()
interpretAssignment (Assignment assignmentLHS expr) = do
  val <- interpretExpr expr
  interpretAssignmentLHS assignmentLHS val
  -- setVar identifier val

interpretAssignmentLHS :: AssignmentLHS -> Val -> Interpret ()
interpretAssignmentLHS (AssignmentLHSIdentifier ident) val = do
  setVar ident val
interpretAssignmentLHS (AssignmentLHSArrayIndex (ArrayIndex arrayIdent expr)) val = do
  idx <- interpretExprToInt expr
  v <- getIdentVec arrayIdent
  liftIO $ MVec.write v (fromIntegral idx - 1) val

interpretForLoop :: ForLoop -> Interpret ()
interpretForLoop (ForLoop (Assignment AssignmentLHSArrayIndex{} _) _ _ _) = do
  fail $
    "In for loop, loop variable is an indexed array, but this is not allowed."
interpretForLoop (ForLoop assignment@(Assignment (AssignmentLHSIdentifier ident) _) direction goalExpr bodyStatements) = do
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
      setVar ident (ValInt (identForModifier i))
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
      maybeIdentVal <- getVar ident
      case maybeIdentVal of
        Nothing -> do
          mapping <- getVarMappings
          fail $
            "The identifier (" <> show ident <>
            ") doesn't have an value in the identifier mapping (" <>
            show mapping <>
            ") in a for loop, even though it definitely should"
        Just (ValBool b) -> do
          fail $
            "The identifier (" <> show ident <>
            ") in a for loop is a boolean and not an integer."
        Just (ValInt i) -> pure i

interpretExpr :: Expr -> Interpret Val
interpretExpr = \case
  ExprArrayIndex (ArrayIndex arrIdent idxExpr) -> do
    idx <- interpretExprToInt idxExpr
    vec <- getIdentVec arrIdent
    liftIO $ MVec.read vec (fromIntegral idx - 1)
  ExprArrayLit exprs -> do
    vals <- traverse interpretExpr exprs
    vec <- Vec.thaw (Vec.fromList vals)
    pure $ ValVector vec
  ExprDivide expr1 expr2 -> do
    int1 <- interpretExprToInt expr1
    int2 <- interpretExprToInt expr2
    pure $ ValInt $ int1 `quot` int2
  ExprGreaterThan expr1 expr2 -> do
    int1 <- interpretExprToInt expr1
    int2 <- interpretExprToInt expr2
    pure $ ValBool $ int1 > int2
  ExprFunCall funCall -> do
    res <- interpretFunCall funCall
    pure res
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
    getIdentVal identifier

getIdentVal :: Identifier -> Interpret Val
getIdentVal ident = do
  maybeVal <- getVar ident
  case maybeVal of
    Nothing -> error $ "No value for identifier: " <> show ident
    Just val -> pure val

getIdentVec :: Identifier -> Interpret (IOVector Val)
getIdentVec ident = do
  val <- getIdentVal ident
  case val of
    ValVector v -> pure v
    val' -> do
      fail $
        "Trying to get identifier " <> show ident <>
        " that should be an array, but it is a " <> valType val'

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
