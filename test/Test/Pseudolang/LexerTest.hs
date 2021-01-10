
module Test.Pseudolang.LexerTest where

import Pseudolang.Prelude

import Text.Megaparsec (eof, errorBundlePretty, mkPos, parse)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Pseudolang.Lexer

parserTest :: (Eq a, Show a) => Parser a -> Text -> a -> IO ()
parserTest p str expectedRes = do
  let eitherRes = parse (p <* eof) "" str
  case eitherRes of
    Right res -> res `shouldBe` expectedRes
    Left err -> expectationFailure $ errorBundlePretty err

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
