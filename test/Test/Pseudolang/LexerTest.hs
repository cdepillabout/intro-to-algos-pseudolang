
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
    it "test5" $ do
      parserTest tokenizer "hello 123"
        [ Token {token = TokIdentifier "hello", startPos = 0, endPos = 5}
        , Token {token = TokInteger 123, startPos = 6, endPos = 9}
        ]
    it "test6" $ do
      parserTest tokenizer "1 + 2"
        [ Token {token = TokInteger 1, startPos = 0, endPos = 1}
        , Token {token = TokPlus, startPos = 2, endPos = 3}
        , Token {token = TokInteger 2, startPos = 4, endPos = 5}
        ]
    it "test7" $ do
      parserTest tokenizer "1 + \"hello dog\""
        [ Token {token = TokInteger 1, startPos = 0, endPos = 1}
        , Token {token = TokPlus, startPos = 2, endPos = 3}
        , Token {token = TokString "hello dog", startPos = 4, endPos = 15}
        ]
    it "test8" $ do
      parserTest tokenizer "1 // hello this comment\n3"
        [ Token {token = TokInteger 1, startPos = 0, endPos = 1}
        , Token {token = TokNewline, startPos = 23, endPos = 24}
        , Token {token = TokInteger 3, startPos = 24, endPos = 25}
        ]
    it "test9" $ do
      parserTest tokenizer "  1 // hello this comment\n  3"
        [ Token {token = TokIndent (mkPos 2), startPos = 0, endPos = 2}
        , Token {token = TokInteger 1, startPos = 2, endPos = 3}
        , Token {token = TokNewline, startPos = 25, endPos = 26}
        , Token {token = TokIndent (mkPos 2), startPos = 26, endPos = 28}
        , Token {token = TokInteger 3, startPos = 28, endPos = 29}
        ]
    it "test10" $ do
      parserTest tokenizer "  // comment on indented line\n  3"
        [ Token {token = TokIndent (mkPos 2), startPos = 0, endPos = 2}
        , Token {token = TokNewline, startPos = 29, endPos = 30}
        , Token {token = TokIndent (mkPos 2), startPos = 30, endPos = 32}
        , Token {token = TokInteger 3, startPos = 32, endPos = 33}
        ]
    it "test11" $ do
      parserTest tokenizer "// comment on first line line\n  3"
        [ Token {token = TokNewline, startPos = 29, endPos = 30}
        , Token {token = TokIndent (mkPos 2), startPos = 30, endPos = 32}
        , Token {token = TokInteger 3, startPos = 32, endPos = 33}
        ]
    it "test12" $ do
      parserTest tokenizer "5\n// comment on non-indented line line\n  3"
        [ Token {token = TokInteger 5, startPos = 0, endPos = 1}
        , Token {token = TokNewline, startPos = 1, endPos = 2}
        , Token {token = TokNewline, startPos = 38, endPos = 39}
        , Token {token = TokIndent (mkPos 2), startPos = 39, endPos = 41}
        , Token {token = TokInteger 3, startPos = 41, endPos = 42}
        ]
