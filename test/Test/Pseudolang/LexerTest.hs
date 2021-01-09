
module Test.Pseudolang.LexerTest where

import Pseudolang.Prelude

import Text.Megaparsec (eof, errorBundlePretty, parse)
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
      parserTest tokenizer "xxx form    for = downto  "
        [ Token {token = TokIdentifier "xxx", startPos = 0, endPos = 3}
        , Token {token = TokIdentifier "form", startPos = 4, endPos = 8}
        , Token {token = TokFor, startPos = 12, endPos = 15}
        , Token {token = TokEquals, startPos = 16, endPos = 17}
        , Token {token = TokDownTo, startPos = 18, endPos = 24}
        ]
