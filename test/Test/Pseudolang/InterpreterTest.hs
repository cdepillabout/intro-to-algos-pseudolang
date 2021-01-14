{-# LANGUAGE QuasiQuotes #-}

module Test.Pseudolang.InterpreterTest where

import Pseudolang.Prelude

import Data.String.Interpolate (__i)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Text.Megaparsec (eof, errorBundlePretty, mkPos, parse)

import Pseudolang.Interpreter
import Pseudolang.Parser (Identifier(..))

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
  describe "Interpreter" $ do
    it "test1" $ do
      let input =
            [__i|
              b = 3
             |]
          expectedMapping = mapFromList [(Identifier "b", ValInt 3)]
      interpreterTest input expectedMapping
    it "test2" $ do
      let input =
            [__i|
              b = 4 < 100
             |]
          expectedMapping = mapFromList [(Identifier "b", ValBool True)]
      interpreterTest input expectedMapping
    it "test3" $ do
      let input =
            [__i|
              b = (4 + 7) < (5 + 3)
             |]
          expectedMapping = mapFromList [(Identifier "b", ValBool False)]
      interpreterTest input expectedMapping
    it "test4" $ do
      let input =
            [__i|
              b = 3
              for i = 0 to 0
                b = b + 1
             |]
          expectedMapping = mapFromList [(Identifier "b", ValInt 3), (Identifier "i", ValInt 0)]
      interpreterTest input expectedMapping
    it "test5" $ do
      let input =
            [__i|
              b = 3
              for i = 0 to 1
                b = b + 1
             |]
          expectedMapping = mapFromList [(Identifier "b", ValInt 4), (Identifier "i", ValInt 1)]
      interpreterTest input expectedMapping
    it "test6" $ do
      let input =
            [__i|
              b = 0
              for i = 0 to 5
                b = b + 1
             |]
          expectedMapping = mapFromList [(Identifier "b", ValInt 5), (Identifier "i", ValInt 5)]
      interpreterTest input expectedMapping
    it "test7" $ do
      let input =
            [__i|
              b = 10
              for i = 0 to 5
                a = i < 3
                b = b + i
                for j = 100 to 103
                  c = j + i
             |]
          expectedMapping =
            mapFromList
              [ (Identifier "a", ValBool False)
              , (Identifier "b", ValInt 20)
              , (Identifier "c", ValInt 106)
              , (Identifier "i", ValInt 5)
              , (Identifier "j", ValInt 103)
              ]
      interpreterTest input expectedMapping
