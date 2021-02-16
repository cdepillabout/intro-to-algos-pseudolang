
module Main where

import Pseudolang.Prelude

import Test.Hspec (hspec)

import qualified Test.Pseudolang.Golden as Golden
import qualified Test.Pseudolang.InterpreterTest as InterpreterTest
import qualified Test.Pseudolang.LexerTest as LexerTest
import qualified Test.Pseudolang.ParserTest as ParserTest
import qualified Test.Pseudolang.ParserNGTest as ParserNGTest

main :: IO ()
main = hspec $ do
  Golden.test
  LexerTest.test
  ParserTest.test
  InterpreterTest.test
  ParserNGTest.test
