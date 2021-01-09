
module Main where

import Pseudolang.Prelude

import Test.Hspec (hspec)

import qualified Test.Pseudolang.LexerTest as LexerTest

main :: IO ()
main = hspec $ do
  LexerTest.test
