{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The \"test\" interpreter.
--
-- The big difference between this and "Pseudolang.Interpreter.Real" is that
-- this actually collects all @print()@ statements in a writer monad and
-- returns them after being evaluated, instead of just printing them to the
-- screen.

module Pseudolang.Interpreter.Test where

import Pseudolang.Prelude

import Text.Megaparsec (eof, errorBundlePretty, parse)
import Text.Pretty.Simple (pShow)

import Pseudolang.Interpreter (MonadInterpret, MonadPrint, InterpState, Val, initialInterpState, interpretAST, printText)
import Pseudolang.Parser (AST, astParser)
import qualified Pseudolang.Lexer as Lexer

newtype Interpret a =
  Interpret { unInterpret :: ExceptT Val (RWST () Text InterpState IO) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadError Val
    , MonadFail
    , MonadInterpret
    , MonadState InterpState
    , MonadWriter Text
    , PrimMonad
    )

runInterpret :: Interpret a -> InterpState -> IO (Either Val a, InterpState, Text)
runInterpret (Interpret m) initInterpState =
  runRWST (runExceptT m) () initInterpState

instance MonadPrint Interpret where
  printText :: Text -> Interpret ()
  printText = tell

data InterpResult = InterpResult InterpState Text

runInterpretToResult :: Interpret a -> InterpState -> IO (Either Val a, InterpResult)
runInterpretToResult m initInterpState = do
  (a, interpState, output) <- runInterpret m initInterpState
  pure (a, InterpResult interpState output)

execInterpretToResult :: Interpret a -> InterpState -> IO InterpResult
execInterpretToResult m initInterpState = do
  (_, res) <- runInterpretToResult m initInterpState
  pure res

parseAndInterpretToResultWithInitial ::
     Text -> InterpState -> IO InterpResult
parseAndInterpretToResultWithInitial inputText initInterpState = do
  let lexerOutput = parse Lexer.tokenizer "" inputText
  case lexerOutput of
    Left err -> do
      fail $ errorBundlePretty err
    Right tokens -> do
      let initialIndentAmount = 0
          parser = runReaderT astParser initialIndentAmount
          eitherAST = parse (parser <* eof) "" tokens
      case eitherAST of
        Right ast -> interpretToResultWithInitial ast initInterpState
        Left err -> fail $ "Error in parsing: " <> (unpack $ pShow err)

parseAndInterpretToResult :: Text -> IO InterpResult
parseAndInterpretToResult text =
  parseAndInterpretToResultWithInitial text initialInterpState

interpretToResultWithInitial :: AST -> InterpState -> IO InterpResult
interpretToResultWithInitial ast initInterpState = do
  execInterpretToResult (interpretAST ast) initInterpState
