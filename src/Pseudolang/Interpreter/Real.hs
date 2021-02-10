{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The \"real\" interpreter.
--
-- The big difference between this and "Pseudolang.Interpreter.Test" is that
-- this actually outputs @print()@ statements to stdout, instead of just
-- collecting the output in a writer monad.

module Pseudolang.Interpreter.Real where

import Pseudolang.Prelude

import Text.Megaparsec (eof, errorBundlePretty, parse)
import Text.Pretty.Simple (pShow)

import Pseudolang.Interpreter (MonadInterpret, MonadRand(..), MonadPrint, InterpState, Val, initialInterpState, interpretAST, printText)
import Pseudolang.Parser (AST, astParser)
import qualified Pseudolang.Lexer as Lexer
import System.Random (Random, mkStdGen, randomRIO, setStdGen)

newtype Interpret a =
  Interpret { unInterpret :: ExceptT Val (StateT InterpState IO) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadError Val
    , MonadFail
    , MonadInterpret
    , MonadState InterpState
    , PrimMonad
    )

runInterpret :: Interpret a -> InterpState -> IO (Either Val a, InterpState)
runInterpret (Interpret m) initInterpState =
  runStateT (runExceptT m) initInterpState

instance MonadPrint Interpret where
  printText :: Text -> Interpret ()
  printText txt = Interpret $ liftIO $ putStr txt

instance MonadRand Interpret where
  getRandR :: Random a => (a, a) -> Interpret a
  getRandR range = Interpret $ liftIO $ randomRIO range

  setSeed :: Int -> Interpret ()
  setSeed i = Interpret $ liftIO $ setStdGen (mkStdGen i)

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
  (_, resInterpState) <- runInterpret (interpretAST ast) initInterpState
  pure resInterpState

interpret :: AST -> IO ()
interpret ast = void $ interpretToInterpState ast
