module Pseudolang.Interpreter where

import Pseudolang.Prelude

import qualified Data.Vector as Vec
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MVec
import System.IO.Unsafe (unsafePerformIO)
import System.Random (Random)

import Pseudolang.Parser

data InterpState = InterpState
  { interpStateVars :: Map Identifier Val
  , interpStateFuns :: Map Identifier FunDef
  }
  -- deriving stock (Eq, Ord, Show)

class MonadPrint m where
  printText :: Text -> m ()

printTextLn :: MonadPrint m => Text -> m ()
printTextLn t = printText $ t <> "\n"

class MonadRand m where
  getRandR :: Random a => (a, a) -> m a
  setSeed :: Int -> m ()

class
  ( MonadState InterpState m
  , MonadError Val m
    -- ^ Used for @return@ statements that have to "throw" a value as the result of
    -- a function call.
  , MonadPrint m
    -- ^ Used for printing to the console (or not to the console and
    -- just holding all the output in a writer monad).
  , PrimMonad m, PrimState m ~ RealWorld
    -- ^ Used for the Data.Vector.thaw operation for creating mutatable vectors.
  , MonadFail m
    -- ^ Used for error messages when the user does something bad.
  , MonadRand m
  ) => MonadInterpret m

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

-- | Integers are equal with themselves
--
-- >>> eqValInt 3 3
-- True
-- >>> eqValInt 3 4
-- False
--
-- Positive infinity and negative infinity are equal with themselves
--
-- >>> eqValInt ValIntPositiveInfinity ValIntPositiveInfinity
-- True
-- >>> eqValInt ValIntNegativeInfinity ValIntNegativeInfinity
-- True
--
-- Nothing else is equal.
--
-- >>> eqValInt 3 ValIntPositiveInfinity
-- False
-- >>> eqValInt ValIntNegativeInfinity 100
-- False
--
-- TODO: I don't know if it makes sense for positive and negative infinity to
-- be equal, but it makes writing unit tests easier.
eqValInt :: ValInt -> ValInt -> Bool
eqValInt (ValIntInteger i1) (ValIntInteger i2) = i1 == i2
eqValInt ValIntPositiveInfinity ValIntPositiveInfinity = True
eqValInt ValIntNegativeInfinity ValIntNegativeInfinity = True
eqValInt _ _ = False

-- | 'compare' for 'ValInt'.
--
-- Integeres are compared normally:
--
-- >>> compareValInt 3 4
-- LT
-- >>> compareValInt 10 10
-- EQ
--
-- When 'ValIntNegativeInfinity' comes first, it is always 'LT', even for itself.
--
-- >>> compareValInt ValIntNegativeInfinity 10
-- LT
-- >>> compareValInt ValIntNegativeInfinity ValIntNegativeInfinity
-- LT
--
-- Otherwise, when 'ValIntNegativeInfinity' comes second, it is always 'GT':
--
-- >>> compareValInt 10 ValIntNegativeInfinity
-- GT
--
-- 'ValIntPositiveInfinity' is the opposite.  When it comes first, it is always
-- 'GT', even for itself..  When it comes second, it is always LT:
--
-- >>> compareValInt ValIntPositiveInfinity 10
-- GT
-- >>> compareValInt ValIntPositiveInfinity ValIntPositiveInfinity
-- GT
-- >>> compareValInt 3 ValIntPositiveInfinity
-- LT
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
minusValInt (ValIntInteger _) ValIntNegativeInfinity =
  -- An integer minus negative infinity is positive infinity.
  ValIntPositiveInfinity
minusValInt (ValIntInteger _) ValIntPositiveInfinity =
  -- An integer minus positive infinity is negative infinity.
  ValIntNegativeInfinity
minusValInt ValIntPositiveInfinity _ =
  -- Positive infinity minus anything is still positive infinity.
  ValIntPositiveInfinity
minusValInt ValIntNegativeInfinity _ =
  -- Negative infinity minus anything is still negative infinity.
  ValIntNegativeInfinity

plusValInt :: ValInt -> ValInt -> ValInt
plusValInt (ValIntInteger i1) (ValIntInteger i2) = ValIntInteger $ i1 + i2
plusValInt (ValIntInteger _) ValIntPositiveInfinity =
  -- An integer plus positive infinity is positive infinity
  ValIntPositiveInfinity
plusValInt (ValIntInteger _) ValIntNegativeInfinity =
  -- An integer plus negative infinity is negative infinity
  ValIntNegativeInfinity
plusValInt ValIntPositiveInfinity _ =
  -- Positive infinity plus anything is still positive infinity.
  ValIntPositiveInfinity
plusValInt ValIntNegativeInfinity _ =
  -- Negative infinity plus anything is still negative infinity.
  ValIntNegativeInfinity

timesValInt :: ValInt -> ValInt -> ValInt
timesValInt (ValIntInteger i1) (ValIntInteger i2) = ValIntInteger $ i1 * i2
timesValInt (ValIntInteger _)  ValIntPositiveInfinity =
  -- Positive infinity times anything is still positive infinity.
  -- TODO: Should there be some logic here about positive infinity times a
  -- negative number is negative infinity?
  ValIntPositiveInfinity
timesValInt (ValIntInteger _)  ValIntNegativeInfinity =
  -- Negative infinity times anything is still negative infinity.
  -- TODO: See above.
  ValIntNegativeInfinity
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
quotValInt (ValIntInteger _) ValIntPositiveInfinity =
  -- Positive infinity div anything is still positive infinity.
  -- TODO: Should there be some logic here about positive infinity div a
  -- negative number is negative infinity?
  ValIntPositiveInfinity
quotValInt (ValIntInteger _) ValIntNegativeInfinity =
  -- Negative infinity div anything is still negative infinity.
  -- TODO: See above.
  ValIntNegativeInfinity
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
negateValInt ValIntNegativeInfinity = ValIntPositiveInfinity

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
  | ValTuple ![Val]
  | ValUnit
  | ValVector (IOVector Val)
  -- deriving stock (Eq, Ord, Show)

instance Show Val where
  show (ValBool b) = show b
  show (ValInt i) = show i
  show (ValString i) = unpack i
  show (ValTuple t) =
    let shownVals = fmap show t
        renderedInnerVals = fold $ intersperse ", " shownVals
    in
    "(" <> renderedInnerVals <> ")"
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
  ValTuple t1 == ValTuple t2 = t1 == t2
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
valType ValTuple{} = "tuple"
valType ValUnit{} = "unit"
valType ValVector{} = "array"

interpretAST :: MonadInterpret m => AST -> m ()
interpretAST (AST topLevels) = for_ topLevels interpretTopLevel

interpretTopLevel :: MonadInterpret m => TopLevel -> m ()
interpretTopLevel = \case
  TopLevelFunDef funDef -> interpretFunDef funDef
  TopLevelStatement statement -> interpretStatement statement

interpretStatements :: MonadInterpret m => [Statement] -> m ()
interpretStatements statements = for_ statements interpretStatement

interpretFunDef :: MonadInterpret m => FunDef -> m ()
interpretFunDef funDef@(FunDef funName _ _) =
  addFunDef funName funDef

interpretStatement :: MonadInterpret m => Statement -> m ()
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

interpretBuiltinPrint :: MonadInterpret m => [Val] -> m Val
interpretBuiltinPrint vals = do
  printTextLn allVals
  pure ValUnit
  where
    shownVals :: [Text]
    shownVals = fmap tshow vals

    allVals :: Text
    allVals = fold $ intersperse " " shownVals

interpretBuiltinFloor :: MonadInterpret m => [Val] -> m Val
interpretBuiltinFloor valArgs = do
  val <- assertOneFunArg "floor" valArgs
  valInt <- assertValIsValInt val
  pure $ ValInt $ floorValInt valInt

interpretBuiltinCeiling :: MonadInterpret m => [Val] -> m Val
interpretBuiltinCeiling valArgs = do
  val <- assertOneFunArg "ceiling" valArgs
  valInt <- assertValIsValInt val
  pure $ ValInt $ ceilingValInt valInt

interpretBuiltinNewArray :: MonadInterpret m => [Val] -> m Val
interpretBuiltinNewArray valArgs = do
  val <- assertOneFunArg "new-array" valArgs
  int <- assertValIsInteger val
  vec <- Vec.thaw (Vec.replicate (fromIntegral int) ValUnit)
  pure $ ValVector vec

interpretBuiltinRandom :: MonadInterpret m => [Val] -> m Val
interpretBuiltinRandom valArgs = do
  (start, end) <- assertTwoFunArgs "Random" valArgs
  startVal <- assertValIsInteger start
  endVal <- assertValIsInteger end
  randVal <- getRandR (startVal, endVal)
  pure $ ValInt $ ValIntInteger randVal

interpretBuiltinSetSeed :: MonadInterpret m => [Val] -> m Val
interpretBuiltinSetSeed valArgs = do
  val <- assertOneFunArg "set-seed" valArgs
  int <- assertValIsInteger val
  setSeed (fromInteger int)
  pure $ ValUnit

assertOneFunArg :: MonadInterpret m => String -> [a] -> m a
assertOneFunArg _ [arg] = pure arg
assertOneFunArg funName args =
  fail $
    funName <> "() function called with " <> show (length args) <>
    " args, but it should have one arg."

assertTwoFunArgs :: MonadInterpret m => String -> [a] -> m (a, a)
assertTwoFunArgs _ [arg1, arg2] = pure (arg1, arg2)
assertTwoFunArgs funName args =
  fail $
    funName <> "() function called with " <> show (length args) <>
    " args, but it should have two args."

interpretBuiltinFunCall :: MonadInterpret m => Identifier -> [Expr] -> m (Maybe Val)
interpretBuiltinFunCall (Identifier builtinFunName) funCallArgs = do
  funCallVals <- traverse interpretExpr funCallArgs
  case builtinFunName of
    "ceiling" -> fmap Just $ interpretBuiltinCeiling funCallVals
    "floor" -> fmap Just $ interpretBuiltinFloor funCallVals
    "new-array" -> fmap Just $ interpretBuiltinNewArray funCallVals
    "print" -> fmap Just $ interpretBuiltinPrint funCallVals
    "Random" -> fmap Just $ interpretBuiltinRandom funCallVals
    "set-seed" -> fmap Just $ interpretBuiltinSetSeed funCallVals
    _ -> pure Nothing

interpretFunCall :: MonadInterpret m => FunCall -> m Val
interpretFunCall (FunCall funName (Tuple funCallArgs)) = do
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

interpretElseIfElse :: MonadInterpret m => ElseIfElse -> m ()
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

interpretIf :: MonadInterpret m => If -> m ()
interpretIf (If conditionExpr thenStatements maybeElseIfElseBlocks) = do
  conditionVal <- interpretExprToBool conditionExpr
  if conditionVal
    then interpretStatements (toList thenStatements)
    else
      case maybeElseIfElseBlocks of
        Nothing -> pure ()
        Just elseIfElseBlocks -> interpretElseIfElse elseIfElseBlocks

interpretAssignment :: MonadInterpret m => Assignment -> m ()
interpretAssignment (Assignment assignmentLHS expr) = do
  val <- interpretExpr expr
  interpretAssignmentLHS assignmentLHS val
  -- setVar identifier val

interpretAssignmentLHS :: MonadInterpret m => AssignmentLHS -> Val -> m ()
interpretAssignmentLHS (AssignmentLHSIdentifier ident) val = do
  setVar ident val
interpretAssignmentLHS (AssignmentLHSArrayIndex (ArrayIndex arrayIdent expr)) val = do
  idx <- interpretExprToInteger expr
  v <- getIdentVec arrayIdent
  MVec.write v (fromIntegral idx - 1) val
interpretAssignmentLHS (AssignmentLHSTuple assignments) val = do
  vals <- assertValIsTuple val
  let lenAssignments = length assignments
      lenVals = length vals
  if lenAssignments /= lenVals
    then
      fail $
        "In assignment statement, trying to assign " <> show lenVals <>
        " values to " <> show lenAssignments <> " tuple elements"
    else do
      let zippedAssignVals = zip assignments vals
      for_ zippedAssignVals \(assignment, val') -> interpretAssignmentLHS assignment val'

interpretForLoop :: forall m. MonadInterpret m => ForLoop -> m ()
interpretForLoop (ForLoop (Assignment AssignmentLHSArrayIndex{} _) _ _ _) = do
  fail $
    "In for loop, loop variable is an indexed array, but this is not allowed."
interpretForLoop (ForLoop (Assignment AssignmentLHSTuple{} _) _ _ _) = do
  fail $
    "In for loop, loop variable is a tuple, but this is not allowed."
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

    loop :: [Statement] -> m ()
    loop [] = do
      -- If there are no more statements left to interpret in the loop body,
      -- then we either increment or decrement the loop identifier, and then loop again.
      i <- getLoopIdentVal
      setVar ident (ValInt (identForModifier i))
      loopWhileNotGoal
    loop (thisStatement : remainingStatements) = do
      interpretStatement thisStatement
      loop remainingStatements

    loopIsGoal :: ValInt -> ValInt -> Bool
    loopIsGoal i goal =
      case direction of
        ForDirectionDownTo -> lessThanValInt i goal
        ForDirectionTo -> greaterThanValInt i goal

    loopWhileNotGoal :: m ()
    loopWhileNotGoal = do
      i <- getLoopIdentVal
      goal <- interpretExprToValInt goalExpr
      if loopIsGoal i goal
        then pure ()
        else loop bodyStatements

    getLoopIdentVal :: m ValInt
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

interpretWhileLoop :: MonadInterpret m => WhileLoop -> m ()
interpretWhileLoop whileLoop@(WhileLoop conditionExpr bodyStatements) = do
  conditionVal <- interpretExprToBool conditionExpr
  if conditionVal
    then do
      interpretStatements bodyStatements
      interpretWhileLoop whileLoop
    else pure ()

interpretExpr :: MonadInterpret m => Expr -> m Val
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
    MVec.read vec (fromIntegral idx - 1)
  ExprArrayLit exprs -> do
    vals <- traverse interpretExpr exprs
    vec <- Vec.thaw (Vec.fromList vals)
    pure $ ValVector vec
  ExprDivide expr1 expr2 -> do
    valInt1 <- interpretExprToValInt expr1
    valInt2 <- interpretExprToValInt expr2
    pure $ ValInt $ quotValInt valInt1 valInt2
  ExprEquals expr1 expr2 -> do
    valInt1 <- interpretExprToValInt expr1
    valInt2 <- interpretExprToValInt expr2
    pure $ ValBool $ eqValInt valInt1 valInt2
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
  ExprTuple tuple -> interpretTuple tuple
  ExprVar identifier -> do
    getIdentVal identifier

-- | Interpret a property access like @A.length@.
--
-- There are only a few available properties.
interpretProperty :: MonadInterpret m => Property -> m Val
interpretProperty (Property ident propIdent) =
  case propIdent of
    (Identifier "length") -> do
      -- .length is only available on vectors.
      vec <- getIdentVec ident
      let vecLength = MVec.length vec
      pure $ ValInt $ ValIntInteger (fromIntegral vecLength)
    (Identifier x) ->
      fail $ "Trying to access property " <> unpack x <> ", but unknown property."

getIdentVal :: MonadInterpret m => Identifier -> m Val
getIdentVal ident = do
  maybeVal <- getVar ident
  case maybeVal of
    Nothing -> fail $ "No value for identifier: " <> show ident
    Just val -> pure val

getIdentVec :: MonadInterpret m => Identifier -> m (IOVector Val)
getIdentVec ident = do
  val <- getIdentVal ident
  case val of
    ValVector v -> pure v
    val' -> do
      fail $
        "Trying to get identifier " <> show ident <>
        " that should be an array, but it is a " <> valType val'

assertValIsValInt :: MonadInterpret m => Val -> m ValInt
assertValIsValInt (ValInt valInt) = pure valInt
assertValIsValInt val = fail $ "Expecting an int, but got a val: " <> show val

assertValIsInteger :: MonadInterpret m => Val -> m Integer
assertValIsInteger val = do
  valInt <- assertValIsValInt val
  case valInt of
    ValIntInteger i -> pure i
    ValIntPositiveInfinity ->
      fail $ "Expecting an integer, but got infinity"
    ValIntNegativeInfinity ->
      fail $ "Expecting an integer, but got -infinity"

assertValIsTuple :: MonadInterpret m => Val -> m [Val]
assertValIsTuple (ValTuple vals) = pure vals
assertValIsTuple val = fail $ "Expecting a tuple, but got a val: " <> show val

interpretTuple :: MonadInterpret m => Tuple -> m Val
interpretTuple (Tuple exprs) = do
  vals <- traverse interpretExpr exprs
  pure $ ValTuple vals

interpretExprToValTuple :: MonadInterpret m => Expr -> m [Val]
interpretExprToValTuple expr = do
  val <- interpretExpr expr
  assertValIsTuple val

interpretExprToValInt :: MonadInterpret m => Expr -> m ValInt
interpretExprToValInt expr = do
  val <- interpretExpr expr
  assertValIsValInt val

interpretExprToInteger :: MonadInterpret m => Expr -> m Integer
interpretExprToInteger expr = do
  val <- interpretExpr expr
  assertValIsInteger val

interpretExprToBool :: MonadInterpret m => Expr -> m Bool
interpretExprToBool expr = do
  val <- interpretExpr expr
  case val of
    ValBool bool' -> pure bool'
    val' -> fail $ "Expecting a bool, but got a val: " <> show val'
