
module Test.Pseudolang.LexerTest where

import Pseudolang.Prelude

import Control.Monad.Fail (fail)
import Text.Megaparsec (eof, errorBundlePretty, mkPos, parse)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Pseudolang.Lexer

parserTest :: (Eq a, Show a) => Parser a -> Text -> a -> IO ()
parserTest parser input expectedRes = do
  res <- parserTest' parser input
  res `shouldBe` expectedRes

parserTest' :: (Eq a, Show a) => Parser a -> Text -> IO a
parserTest' parser input = do
  let eitherRes = parse (parser <* eof) "" input
  case eitherRes of
    Right res -> pure res
    Left err -> do
      expectationFailure $ errorBundlePretty err
      fail "we should never reach this line because expectationFailure will throw an exception"

test :: Spec
test =
  describe "Lexer" $ do
    it "test1" $ do
      parserTest tokenizer "for  " [Token TokFor 0 3]
    it "test2" $ do
      parserTest tokenizer "for   = "
        [ Token TokFor 0 3
        , Token TokEquals 6 7
        ]
    it "test3" $ do
      parserTest tokenizer "xx-x- form  -  for = downto , "
        [ Token {token = TokIdentifier "xx-x-", startPos = 0, endPos = 5}
        , Token {token = TokIdentifier "form", startPos = 6, endPos = 10}
        , Token {token = TokMinus, startPos = 12, endPos = 13}
        , Token {token = TokFor, startPos = 15, endPos = 18}
        , Token {token = TokEquals, startPos = 19, endPos = 20}
        , Token {token = TokDownTo, startPos = 21, endPos = 27}
        , Token {token = TokComma, startPos = 28, endPos = 29}
        ]
    it "test4" $ do
      parserTest tokenizer "hello\n  bye"
        [ Token {token = TokIdentifier "hello", startPos = 0, endPos = 5}
        , Token {token = TokNewline, startPos = 5, endPos = 6}
        , Token {token = TokIndent (mkPos 2), startPos = 6, endPos = 8}
        , Token {token = TokIdentifier "bye", startPos = 8, endPos = 11}
        ]
    it "test4" $ do
      parserTest tokenizer "hello 123"
        [ Token {token = TokIdentifier "hello", startPos = 0, endPos = 5}
        , Token {token = TokInteger 123, startPos = 6, endPos = 9}
        ]
    it "test4" $ do
      parserTest tokenizer "1 + 2"
        [ Token {token = TokInteger 1, startPos = 0, endPos = 1}
        , Token {token = TokPlus, startPos = 2, endPos = 3}
        , Token {token = TokInteger 2, startPos = 4, endPos = 5}
        ]
