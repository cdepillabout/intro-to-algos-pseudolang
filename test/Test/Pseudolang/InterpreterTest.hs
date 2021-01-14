{-# LANGUAGE QuasiQuotes #-}

module Test.Pseudolang.InterpreterTest where

import Pseudolang.Prelude

import Data.String.Interpolate (__i)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Text.Megaparsec (eof, errorBundlePretty, mkPos, parse)

interpreterTest :: Text -> (Map Identifier Val) -> IO ()
interpreterTest input expectedMapping = do
  -- let initialIndentAmount = 0
  --     parser = runReaderT readerTParser initialIndentAmount
  --     eitherRes = parse (parser <* eof) "" input
  -- case eitherRes of
  --   Right res -> res `shouldBe` expectedRes
  --   Left err -> expectationFailure $ unpack $ pShow err
  resMapping <- parseAndInterpretToMapping input
  resMapping `shouldBe` expectedMapping

test :: Spec
test =
  describe "Parser" $ do
    it "test1" $ do
      let input =
            [__i|
              b = 3
             |]
          expectedMapping = mapFromList [(Identifier "b", ValInt 3)]
      interpreterTest input expectedMapping
