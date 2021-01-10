
module Main where

import Pseudolang.Prelude

import Test.Hspec (hspec)

import qualified Test.Pseudolang.LexerTest as LexerTest
import qualified Test.Pseudolang.ParserTest as ParserTest

main :: IO ()
main = hspec $ do
  LexerTest.test
  ParserTest.test
