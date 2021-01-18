{-# LANGUAGE QuasiQuotes #-}

module Test.Pseudolang.InterpreterTest where

import Pseudolang.Prelude

import Data.String.Interpolate (__i)
import qualified Data.Vector as Vec
import Data.Vector.Mutable (IOVector)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Text.Megaparsec (eof, errorBundlePretty, mkPos, parse)

import Pseudolang.Interpreter
import Pseudolang.Parser (Identifier(..))

thawFromList :: MonadIO m => [Val] -> m (IOVector Val)
thawFromList l = liftIO $ Vec.thaw $ Vec.fromList l

interpreterTest :: Text -> (Map Identifier Val) -> IO ()
interpreterTest input expectedVarMapping = do
  resInterpState <- parseAndInterpretToInterpState input
  let resVarMapping = interpStateVars resInterpState
  resVarMapping `shouldBe` expectedVarMapping

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
    it "test8" $ do
      let input =
            [__i|
              fun Insertion-Sort(a, x)
                a = 10
                return x + 1
              x = 100
              b = Insertion-Sort(1, x)
              c = Insertion-Sort(3, b)
             |]
          expectedMapping =
            mapFromList
              [ (Identifier "b", ValInt 101)
              , (Identifier "c", ValInt 102)
              , (Identifier "x", ValInt 100)
              ]
      interpreterTest input expectedMapping
    it "test9" $ do
      let input =
            [__i|
              fun ppp(a)
                return a + 1
              x = 100
              b = ppp(3) + ppp(x)
             |]
          expectedMapping =
            mapFromList
              [ (Identifier "b", ValInt 105)
              , (Identifier "x", ValInt 100)
              ]
      interpreterTest input expectedMapping
    it "test10" $ do
      aVec <- thawFromList [ValInt 1, ValInt 2, ValInt 3]
      let input =
            [__i|
              A = [1,2,3]
             |]
          expectedMapping =
            mapFromList
              [ (Identifier "A", ValVector aVec)
              ]
      interpreterTest input expectedMapping
    it "test11" $ do
      aVec <- thawFromList [ValInt 100, ValInt 5, ValInt 301]
      let input =
            [__i|
              A = [100, 200, 301]
              b = 3
              A[b - 1] = 5
              x = A[3] + A[2]
             |]
          expectedMapping =
            mapFromList
              [ (Identifier "A", ValVector aVec)
              , (Identifier "b", ValInt 3)
              , (Identifier "x", ValInt 306)
              ]
      interpreterTest input expectedMapping
    it "test12" $ do
      aVec <- thawFromList [ValInt 100, ValInt 200, ValInt 301]
      let input =
            [__i|
              A = [100, 200, 301]
              b = A.length + 1
             |]
          expectedMapping =
            mapFromList
              [ (Identifier "A", ValVector aVec)
              , (Identifier "b", ValInt 4)
              ]
      interpreterTest input expectedMapping
