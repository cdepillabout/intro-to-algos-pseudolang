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

data ValInt
  = ValIntInteger !Integer
  | ValIntPositiveInfinity
  | ValIntNegativeInfinity

eqValInt :: ValInt -> ValInt -> Bool
eqValInt (ValIntInteger i1) (ValIntInteger i2) = i1 == i2
eqValInt ValIntPositiveInfinity ValIntPositiveInfinity =
  -- TODO: Infinity is equal with itself.
  -- This probably shouldn't be the case(?), but we currently
  -- make use of this fact in the unit tests.
  True
eqValInt ValIntNegativeInfinity ValIntNegativeInfinity =
  -- TODO: Infinity is equal with itself.
  -- This probably shouldn't be the case(?), but we currently
  -- make use of this fact in the unit tests.
  True
eqValInt _ _ = False

compareValInt :: ValInt -> ValInt -> Ordering
compareValInt (ValIntInteger i1) (ValIntInteger i2) = compare i1 i2
compareValInt ValIntNegativeInfinity _ =
  -- Negative infinity is less than anything, even itself.
  LT
compareValInt _ ValIntNegativeInfinity =
  -- Anything is greater than negative infinity.
  GT
compareValInt ValIntPositiveInfinity _ =
  -- Positive infinity is greater than everything, even itself.
  GT
compareValInt _ ValIntPositiveInfinity =
  -- Anything is less than positive infinity.
  LT

greaterThanValInt :: ValInt -> ValInt -> Bool
greaterThanValInt v1 v2 =
  case compareValInt v1 v2 of
    GT -> True
    _ -> False

greaterThanOrEqualToValInt :: ValInt -> ValInt -> Bool
greaterThanOrEqualToValInt v1 v2 =
  case compareValInt v1 v2 of
    GT -> True
    EQ -> True
    _ -> False

lessThanValInt :: ValInt -> ValInt -> Bool
lessThanValInt v1 v2 =
  case compareValInt v1 v2 of
    LT -> True
    _ -> False

lessThanOrEqualToValInt :: ValInt -> ValInt -> Bool
lessThanOrEqualToValInt v1 v2 =
  case compareValInt v1 v2 of
    LT -> True
    EQ -> True
    _ -> False

minusValInt :: ValInt -> ValInt -> ValInt
minusValInt (ValIntInteger i1) (ValIntInteger i2) = ValIntInteger $ i1 - i2
minusValInt ValIntPositiveInfinity _ =
  -- Positive infinity minus anything is still positive infinity.
  ValIntPositiveInfinity
minusValInt ValIntNegativeInfinity _ =
  -- Negative infinity minus anything is still negative infinity.
  ValIntNegativeInfinity

plusValInt :: ValInt -> ValInt -> ValInt
plusValInt (ValIntInteger i1) (ValIntInteger i2) = ValIntInteger $ i1 + i2
plusValInt ValIntPositiveInfinity _ =
  -- Positive infinity plus anything is still positive infinity.
  ValIntPositiveInfinity
plusValInt ValIntNegativeInfinity _ =
  -- Negative infinity plus anything is still negative infinity.
  ValIntNegativeInfinity

timesValInt :: ValInt -> ValInt -> ValInt
timesValInt (ValIntInteger i1) (ValIntInteger i2) = ValIntInteger $ i1 * i2
timesValInt ValIntPositiveInfinity _ =
  -- Positive infinity times anything is still positive infinity.
  -- TODO: Should there be some logic here about positive infinity times a
  -- negative number is negative infinity?
  ValIntPositiveInfinity
timesValInt ValIntNegativeInfinity _ =
  -- Negative infinity times anything is still negative infinity.
  -- TODO: See above.
  ValIntNegativeInfinity

quotValInt :: ValInt -> ValInt -> ValInt
quotValInt (ValIntInteger i1) (ValIntInteger i2) = ValIntInteger $ i1 `quot` i2
quotValInt ValIntPositiveInfinity _ =
  -- Positive infinity div anything is still positive infinity.
  -- TODO: Should there be some logic here about positive infinity div a
  -- negative number is negative infinity?
  ValIntPositiveInfinity
quotValInt ValIntNegativeInfinity _ =
  -- Negative infinity div anything is still negative infinity.
  -- TODO: See above.
  ValIntNegativeInfinity

negateValInt :: ValInt -> ValInt
negateValInt (ValIntInteger i) = ValIntInteger $ negate i
-- Negating positive and negative infinity flip them around.
negateValInt ValIntPositiveInfinity = ValIntNegativeInfinity
negateValInt ValIntPositiveInfinity = ValIntNegativeInfinity

absValInt :: ValInt -> ValInt
absValInt (ValIntInteger i) = ValIntInteger $ abs i
absValInt _ = ValIntPositiveInfinity

signumValInt :: ValInt -> ValInt
signumValInt (ValIntInteger i) = ValIntInteger $ signum i
signumValInt ValIntPositiveInfinity = ValIntInteger 1
signumValInt ValIntNegativeInfinity = ValIntInteger (-1)

floorValInt :: ValInt -> ValInt
floorValInt = id

ceilingValInt :: ValInt -> ValInt
ceilingValInt = id

instance Num ValInt where
  (+) = plusValInt
  (-) = minusValInt
  negate = negateValInt
  (*) = timesValInt
  abs = absValInt
  signum = signumValInt
  fromInteger = ValIntInteger

instance Show ValInt where
  show (ValIntInteger i) = show i
  show ValIntPositiveInfinity = "infinity"
  show ValIntNegativeInfinity = "-infinity"

data Val
  = ValBool !Bool
  | ValInt !ValInt
  | ValString !Text
  | ValUnit
  | ValVector (IOVector Val)
  -- deriving stock (Eq, Ord, Show)

instance Show Val where
  show (ValBool b) = show b
  show (ValInt i) = show i
  show (ValString i) = unpack i
  show ValUnit = "unit"
  show (ValVector v) = show g
    where
      g :: Vec.Vector Val
      g = unsafePerformIO $ Vec.freeze v
      {-# NOINLINE g #-}

instance Eq Val where
  ValBool b1 == ValBool b2 = b1 == b2
  ValInt i1 == ValInt i2 = eqValInt i1 i2
  ValString s1 == ValString s2 = s1 == s2
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
  _ == _ = False

valType :: Val -> String
valType ValBool{} = "bool"
valType ValInt{} = "int"
valType ValString{} = "string"
valType ValUnit{} = "unit"
valType ValVector{} = "array"

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
        Left err -> fail $ "Error in parsing: " <> (unpack $ pShow err)

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
  StatementExpr expr -> void $ interpretExpr expr
  StatementForLoop forLoop -> interpretForLoop forLoop
  StatementFunCall funCall -> void $ interpretFunCall funCall
  StatementIf if' -> interpretIf if'
  StatementReturn expr -> do
    val <- interpretExpr expr
    throwError val
  StatementWhileLoop whileLoop -> interpretWhileLoop whileLoop

interpretBuiltinPrint :: [Val] -> Interpret Val
interpretBuiltinPrint [] = do
  putStrLn ""
  pure ValUnit
interpretBuiltinPrint (v:vals) = do
  putStr $ tshow v <> " "
  interpretBuiltinPrint vals

interpretBuiltinFloor :: [Val] -> Interpret Val
interpretBuiltinFloor valArgs = do
  val <- assertOneFunArg "floor" valArgs
  valInt <- assertValIsValInt val
  pure $ ValInt $ floorValInt valInt

interpretBuiltinCeiling :: [Val] -> Interpret Val
interpretBuiltinCeiling valArgs = do
  val <- assertOneFunArg "ceiling" valArgs
  valInt <- assertValIsValInt val
  pure $ ValInt $ ceilingValInt valInt

interpretBuiltinNewArray :: [Val] -> Interpret Val
interpretBuiltinNewArray valArgs = do
  val <- assertOneFunArg "new-array" valArgs
  int <- assertValIsInteger val
  vec <- Vec.thaw (Vec.replicate (fromIntegral int) ValUnit)
  pure $ ValVector vec

assertOneFunArg :: String -> [a] -> Interpret a
assertOneFunArg _ [arg] = pure arg
assertOneFunArg funName args =
  fail $
    funName <> "() function called with " <> show (length args) <>
    " args, but it should have one arg."

interpretBuiltinFunCall :: Identifier -> [Expr] -> Interpret (Maybe Val)
interpretBuiltinFunCall (Identifier builtinFunName) funCallArgs = do
  funCallVals <- traverse interpretExpr funCallArgs
  case builtinFunName of
    "ceiling" -> fmap Just $ interpretBuiltinCeiling funCallVals
    "floor" -> fmap Just $ interpretBuiltinFloor funCallVals
    "new-array" -> fmap Just $ interpretBuiltinNewArray funCallVals
    "print" -> fmap Just $ interpretBuiltinPrint funCallVals
    _ -> pure Nothing

interpretFunCall :: FunCall -> Interpret Val
interpretFunCall (FunCall funName funCallArgs) = do
  maybeFunName <- getFunDef funName
  case maybeFunName of
    Nothing -> do
      maybeVal <- interpretBuiltinFunCall funName funCallArgs
      case maybeVal of
        Nothing ->
          fail $
            "Trying to call the function (" <> show funName <>
            "), but it doesn't have an value in the known function mappings."
        Just val -> pure val
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

interpretElseIfElse :: ElseIfElse -> Interpret ()
interpretElseIfElse (ElseIfElse elseIfs elseStatements) =
  case elseIfs of
    (ElseIf condition elseIfStatements : moreElseIfs) -> do
      conditionVal <- interpretExprToBool condition
      if conditionVal
        then interpretStatements (toList elseIfStatements)
        else interpretElseIfElse (ElseIfElse moreElseIfs elseStatements)
    [] ->
      -- No elseif conditions, so we just execute the else statements
      interpretStatements (toList elseStatements)

interpretIf :: If -> Interpret ()
interpretIf (If conditionExpr thenStatements maybeElseIfElseBlocks) = do
  conditionVal <- interpretExprToBool conditionExpr
  if conditionVal
    then interpretStatements (toList thenStatements)
    else
      case maybeElseIfElseBlocks of
        Nothing -> pure ()
        Just elseIfElseBlocks -> interpretElseIfElse elseIfElseBlocks

interpretAssignment :: Assignment -> Interpret ()
interpretAssignment (Assignment assignmentLHS expr) = do
  val <- interpretExpr expr
  interpretAssignmentLHS assignmentLHS val
  -- setVar identifier val

interpretAssignmentLHS :: AssignmentLHS -> Val -> Interpret ()
interpretAssignmentLHS (AssignmentLHSIdentifier ident) val = do
  setVar ident val
interpretAssignmentLHS (AssignmentLHSArrayIndex (ArrayIndex arrayIdent expr)) val = do
  idx <- interpretExprToInteger expr
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
    --
    -- If the identifier ever becomes infinity, just keep that value.
    identForModifier :: ValInt -> ValInt
    identForModifier ValIntPositiveInfinity = ValIntPositiveInfinity
    identForModifier ValIntNegativeInfinity = ValIntNegativeInfinity
    identForModifier (ValIntInteger i) =
      ValIntInteger $
        case direction of
          ForDirectionDownTo -> i - 1
          ForDirectionTo -> i + 1

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
      goal <- interpretExprToValInt goalExpr
      if greaterThanValInt i goal
        then pure ()
        else loop bodyStatements

    getLoopIdentVal :: Interpret ValInt
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
        Just (ValInt i) -> pure i
        Just val -> do
          fail $
            "The identifier (" <> show ident <>
            ") in a for loop is a " <> show (valType val) <> " and not an integer."

interpretWhileLoop :: WhileLoop -> Interpret ()
interpretWhileLoop whileLoop@(WhileLoop conditionExpr bodyStatements) = do
  conditionVal <- interpretExprToBool conditionExpr
  if conditionVal
    then do
      interpretStatements bodyStatements
      interpretWhileLoop whileLoop
    else pure ()

interpretExpr :: Expr -> Interpret Val
interpretExpr = \case
  ExprAnd expr1 expr2 -> do
    -- We need to evaluate the second expression lazily.
    bool1 <- interpretExprToBool expr1
    if bool1 == False
      then pure $ ValBool False
      else do
        bool2 <- interpretExprToBool expr2
        pure $ ValBool bool2
  ExprArrayIndex (ArrayIndex arrIdent idxExpr) -> do
    idx <- interpretExprToInteger idxExpr
    vec <- getIdentVec arrIdent
    liftIO $ MVec.read vec (fromIntegral idx - 1)
  ExprArrayLit exprs -> do
    vals <- traverse interpretExpr exprs
    vec <- Vec.thaw (Vec.fromList vals)
    pure $ ValVector vec
  ExprDivide expr1 expr2 -> do
    valInt1 <- interpretExprToValInt expr1
    valInt2 <- interpretExprToValInt expr2
    pure $ ValInt $ quotValInt valInt1 valInt2
  ExprGreaterThan expr1 expr2 -> do
    valInt1 <- interpretExprToValInt expr1
    valInt2 <- interpretExprToValInt expr2
    pure $ ValBool $ greaterThanValInt valInt1 valInt2
  ExprGreaterThanOrEqualTo expr1 expr2 -> do
    valInt1 <- interpretExprToValInt expr1
    valInt2 <- interpretExprToValInt expr2
    pure $ ValBool $ greaterThanOrEqualToValInt valInt1 valInt2
  ExprFunCall funCall -> do
    res <- interpretFunCall funCall
    pure res
  ExprInfinity -> pure $ ValInt ValIntPositiveInfinity
  ExprInteger int -> pure $ ValInt $ ValIntInteger int
  ExprLessThan expr1 expr2 -> do
    valInt1 <- interpretExprToValInt expr1
    valInt2 <- interpretExprToValInt expr2
    pure $ ValBool $ lessThanValInt valInt1 valInt2
  ExprLessThanOrEqualTo expr1 expr2 -> do
    valInt1 <- interpretExprToValInt expr1
    valInt2 <- interpretExprToValInt expr2
    pure $ ValBool $ lessThanOrEqualToValInt valInt1 valInt2
  ExprMinus expr1 expr2 -> do
    valInt1 <- interpretExprToValInt expr1
    valInt2 <- interpretExprToValInt expr2
    pure $ ValInt $ minusValInt valInt1 valInt2
  ExprNegate expr -> do
    valInt <- interpretExprToValInt expr
    pure $ ValInt $ negateValInt valInt
  ExprOr expr1 expr2 -> do
    bool1 <- interpretExprToBool expr1
    -- We need to evaluate the second expression lazily.
    if bool1
      then pure $ ValBool True
      else do
        bool2 <- interpretExprToBool expr2
        pure $ ValBool bool2
  ExprParens expr -> interpretExpr expr
  ExprPlus expr1 expr2 -> do
    valInt1 <- interpretExprToValInt expr1
    valInt2 <- interpretExprToValInt expr2
    pure $ ValInt $ plusValInt valInt1 valInt2
  ExprProperty prop -> interpretProperty prop
  ExprString str -> pure $ ValString str
  ExprTimes expr1 expr2 -> do
    valInt1 <- interpretExprToValInt expr1
    valInt2 <- interpretExprToValInt expr2
    pure $ ValInt $ timesValInt valInt1 valInt2
  ExprVar identifier -> do
    getIdentVal identifier

-- | Interpret a property access like @A.length@.
--
-- There are only a few available properties.
interpretProperty :: Property -> Interpret Val
interpretProperty (Property ident propIdent) =
  case propIdent of
    (Identifier "length") -> do
      -- .length is only available on vectors.
      vec <- getIdentVec ident
      let vecLength = MVec.length vec
      pure $ ValInt $ ValIntInteger (fromIntegral vecLength)
    (Identifier x) ->
      fail $ "Trying to access property " <> unpack x <> ", but unknown property."

getIdentVal :: Identifier -> Interpret Val
getIdentVal ident = do
  maybeVal <- getVar ident
  case maybeVal of
    Nothing -> fail $ "No value for identifier: " <> show ident
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

assertValIsValInt :: Val -> Interpret ValInt
assertValIsValInt (ValInt valInt) = pure valInt
assertValIsValInt val = fail $ "Expecting an int, but got a val: " <> show val

assertValIsInteger :: Val -> Interpret Integer
assertValIsInteger val = do
  valInt <- assertValIsValInt val
  case valInt of
    ValIntInteger i -> pure i
    ValIntPositiveInfinity ->
      fail $ "Expecting an integer, but got infinity"
    ValIntNegativeInfinity ->
      fail $ "Expecting an integer, but got -infinity"

interpretExprToValInt :: Expr -> Interpret ValInt
interpretExprToValInt expr = do
  val <- interpretExpr expr
  assertValIsValInt val

interpretExprToInteger :: Expr -> Interpret Integer
interpretExprToInteger expr = do
  val <- interpretExpr expr
  assertValIsInteger val

interpretExprToBool :: Expr -> Interpret Bool
interpretExprToBool expr = do
  val <- interpretExpr expr
  case val of
    ValBool bool' -> pure bool'
    val' -> fail $ "Expecting a bool, but got a val: " <> show val'
