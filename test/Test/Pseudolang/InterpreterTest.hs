{-# LANGUAGE QuasiQuotes #-}

module Test.Pseudolang.InterpreterTest where

import Pseudolang.Prelude

import Data.String.Interpolate (__i)
import qualified Data.Vector as Vec
import Data.Vector.Mutable (IOVector)
import Test.Hspec (Spec, describe, it, shouldBe)

import Pseudolang.Interpreter
import Pseudolang.Interpreter.Test (InterpResult(InterpResult), parseAndInterpretToResult)
import Pseudolang.Parser (Identifier(..))
import Test.Pseudolang.Utils (_i)

thawFromList :: MonadIO m => [Val] -> m (IOVector Val)
thawFromList l = liftIO $ Vec.thaw $ Vec.fromList l

interpreterAndOutputTest :: Text -> Map Identifier Val -> Text -> IO ()
interpreterAndOutputTest inputProgram expectedVarMapping expectedOutput = do
  InterpResult resInterpState resOutput <- parseAndInterpretToResult inputProgram
  let resVarMapping = interpStateVars resInterpState
  resVarMapping `shouldBe` expectedVarMapping
  resOutput `shouldBe` expectedOutput

interpreterTest :: Text -> Map Identifier Val -> IO ()
interpreterTest inputProgram expectedVarMapping = do
  InterpResult resInterpState _ <- parseAndInterpretToResult inputProgram
  let resVarMapping = interpStateVars resInterpState
  resVarMapping `shouldBe` expectedVarMapping

outputTest :: Text -> Text -> IO ()
outputTest inputProgram expectedOutput = do
  InterpResult _ resOutput <- parseAndInterpretToResult inputProgram
  resOutput `shouldBe` expectedOutput

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
              for i = 0 to -1
                b = b + 1
             |]
          expectedMapping = mapFromList [(Identifier "b", ValInt 3), (Identifier "i", ValInt 0)]
      interpreterTest input expectedMapping
    it "test5" $ do
      let input =
            [__i|
              b = 3
              for i = 0 to 0
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
          expectedMapping = mapFromList [(Identifier "b", ValInt 6), (Identifier "i", ValInt 6)]
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
              , (Identifier "b", ValInt 25)
              , (Identifier "c", ValInt 108)
              , (Identifier "i", ValInt 6)
              , (Identifier "j", ValInt 104)
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
    it "test13" $ do
      let input =
            [__i|
              a = 1 > 3
              b = 2 < 10
              w = a and b
              x = b and a
              y = a or b
              z = b or a
             |]
          expectedMapping =
            mapFromList
              [ (Identifier "a", ValBool False)
              , (Identifier "b", ValBool True)
              , (Identifier "w", ValBool False)
              , (Identifier "x", ValBool False)
              , (Identifier "y", ValBool True)
              , (Identifier "z", ValBool True)
              ]
      interpreterTest input expectedMapping
    it "test14" $ do
      let input =
            [__i|
              a = 1
              while a < 5
                a = a + 1
             |]
          expectedMapping =
            mapFromList
              [ (Identifier "a", ValInt 5)
              ]
      interpreterTest input expectedMapping
    it "test15" $ do
      let input =
            [__i|
              a = 3

              fun I()
                x = 4

                return 300

              b = I()
             |]
          expectedMapping =
            mapFromList
              [ ("a", ValInt 3)
              , ("b", ValInt 300)
              ]
      interpreterTest input expectedMapping
    it "test16" $ do
      aVec <- thawFromList [ValInt 1, ValInt 2, ValInt 3, ValInt 4, ValInt 5, ValInt 6]
      let input =
            [__i|
              fun Insertion-Sort(A)
                for j = 2 to A.length
                  key = A[j]
                  // Insert A[j] into the sorted sequence A[1..j - 1]
                  i = j - 1
                  while i > 0 and A[i] > key
                    A[i + 1] = A[i]
                    i = i - 1
                  A[i + 1] = key

              A = [5, 2, 4, 6, 1, 3]
              Insertion-Sort(A)
             |]
          expectedMapping =
            mapFromList
              [ ("A", ValVector aVec)
              ]
      interpreterTest input expectedMapping
    it "test17" $ do
      aVec <- thawFromList [ValInt 5, ValInt 2, ValInt 4, ValInt 6, ValInt 1, ValInt 3]
      let input =
            [__i|
              A = [5, 2, 4, 6, 1, 3]
              for j = 2 to A.length
                key = A[j]
                i = j - 1
             |]
          expectedMapping =
            mapFromList
              [ ("A", ValVector aVec)
              , ("j", ValInt 7)
              , ("i", ValInt 5)
              , ("key", ValInt 3)
              ]
      interpreterTest input expectedMapping
    it "test18" $ do
      aVec <- thawFromList [ValInt 1, ValInt 2, ValInt 3, ValInt 4, ValInt 5, ValInt 6]
      let input =
            [__i|
              A = [5, 2, 4, 6, 1, 3]
              for j = 2 to A.length
                key = A[j]
                i = j - 1
                while i > 0 and A[i] > key
                  A[i + 1] = A[i]
                  i = i - 1
                A[i + 1] = key
             |]
          expectedMapping =
            mapFromList
              [ ("A", ValVector aVec)
              , ("j", ValInt 7)
              , ("i", ValInt 2)
              , ("key", ValInt 3)
              ]
      interpreterTest input expectedMapping
    it "test19" $ do
      let input =
            [__i|
              A = "hello oooo"
             |]
          expectedMapping =
            mapFromList
              [ ("A", ValString "hello oooo")
              ]
      interpreterTest input expectedMapping
    it "test20" $ do
      let input =
            [__i|
              x = 3
              if x > 5
                b = 4
              elseif x > 100
                ddddddd = 2000
              else c = 100
                d = 20
             |]
          expectedMapping =
            mapFromList
              [ ("x", ValInt 3)
              , ("c", ValInt 100)
              , ("d", ValInt 20)
              ]
      interpreterTest input expectedMapping
    it "test21" $ do
      let input =
            [__i|
              x = 3
              if x < 5
                b = 4
              elseif x > 100
                ddddddd = 2000
              else c = 100
                d = 20
             |]
          expectedMapping =
            mapFromList
              [ ("x", ValInt 3)
              , ("b", ValInt 4)
              ]
      interpreterTest input expectedMapping
    it "test22" $ do
      let input =
            [__i|
              x = 3
              if x > 5
                b = 4
              elseif x < 100
                ddd = 2000
              else c = 100
                d = 20
             |]
          expectedMapping =
            mapFromList
              [ ("x", ValInt 3)
              , ("ddd", ValInt 2000)
              ]
      interpreterTest input expectedMapping
    it "test23" $ do
      let input =
            [__i|
              x = infinity
              y = -infinity
              z = infinity + 3
             |]
          expectedMapping =
            mapFromList
              [ ("x", ValInt ValIntPositiveInfinity)
              , ("y", ValInt ValIntNegativeInfinity)
              , ("z", ValInt ValIntPositiveInfinity)
              ]
      interpreterTest input expectedMapping
    it "test24" $ do
      let input =
            [__i|
              x = floor(3)
              y = ceiling(10)
             |]
          expectedMapping =
            mapFromList
              [ ("x", ValInt 3)
              , ("y", ValInt 10)
              ]
      interpreterTest input expectedMapping
    it "test25" $ do
      lVec <- thawFromList [ValUnit, ValUnit, ValUnit, ValUnit]
      let input =
            [__i|
              n1 = 4
              L = new-array(n1)
             |]
          expectedMapping =
            mapFromList
              [ ("n1", ValInt 4)
              , ("L", ValVector lVec)
              ]
      interpreterTest input expectedMapping
    it "test26" $ do
      aVec <- thawFromList [ValInt 1, ValInt 2, ValInt 2, ValInt 3, ValInt 4, ValInt 5, ValInt 6, ValInt 7]
      lVec <- thawFromList [ValInt 2, ValInt 4, ValInt 5, ValInt 7, ValInt ValIntPositiveInfinity]
      rVec <- thawFromList [ValInt 1, ValInt 2, ValInt 3, ValInt 6, ValInt ValIntPositiveInfinity]
      let input =
            [__i|
              A = [2,4,5,7,1,2,3,6]
              p = 1
              q = 4
              r = 8
              n1 = q - p + 1
              n2 = r - q
              L = new-array(n1 + 1)
              R = new-array(n2 + 1)
              for i = 1 to n1
                L[i] = A[p + i - 1]
              for j = 1 to n2
                R[j] = A[q + j]
              L[n1 + 1] = infinity
              R[n2 + 1] = infinity
              i = 1
              j = 1
              for k = p to r
                if L[i] <= R[j]
                  A[k] = L[i]
                  i = i + 1
                else A[k] = R[j]
                  j = j + 1
             |]
          expectedMapping =
            mapFromList
              [ ("A", ValVector aVec)
              , ("p", ValInt 1)
              , ("q", ValInt 4)
              , ("r", ValInt 8)
              , ("n1", ValInt 4)
              , ("n2", ValInt 4)
              , ("L", ValVector lVec)
              , ("R", ValVector rVec)
              , ("i", ValInt 5)
              , ("j", ValInt 5)
              , ("k", ValInt 9)
              ]
      interpreterTest input expectedMapping
    it "test27" $ do
      let input =
            [__i|
              for x = 1 to 4
                print(x)
             |]
          expectedOutput =
            [_i|1
              2
              3
              4
              |]
      outputTest input expectedOutput
    it "test28" $ do
      let input =
            [__i|
              x = (1, 3 + 4, 7)
             |]
          expectedMapping =
            mapFromList
              [ ("x", ValTuple [ValInt 1, ValInt 7, ValInt 7])
              ]
      interpreterTest input expectedMapping
    it "test29" $ do
      let input =
            [__i|
              x = 3
              print((1, (x, x), [3, 4, 5]), "hello")
             |]
          expectedOutput = "(1, (3, 3), [3,4,5]) hello\n"
      outputTest input expectedOutput
    it "test30" $ do
      aVec <- thawFromList [ValInt 4]
      let input =
            [__i|
              A = new-array(1)
              ((x, y), (z, A[1])) = ((1, 2), (3, 4))
             |]
          expectedMapping =
            mapFromList
              [ ("A", ValVector aVec)
              , ("x", ValInt 1)
              , ("y", ValInt 2)
              , ("z", ValInt 3)
              ]
      interpreterTest input expectedMapping
    it "test31" $ do
      let input =
            [__i|
              b = 3
              c = 1 == 3
              for i = 3 downto 1
                b = b + 1
             |]
          expectedMapping =
            mapFromList
              [ (Identifier "b", ValInt 6)
              , (Identifier "c", ValBool False)
              , (Identifier "i", ValInt 0)
              ]
      interpreterTest input expectedMapping
    it "test32" $ do
      let input =
            [__i|
              set-seed(3)
              print(Random(1, 1000))
              set-seed(1)
              print(Random(0, 1))
              set-seed(100561)
              print(Random(0, 1))
             |]
          expectedOutput = "364\n1\n0\n"
      outputTest input expectedOutput
