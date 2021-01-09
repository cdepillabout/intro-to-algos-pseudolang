
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
      parserTest symForParser "for  " (Token TokFor 0 3)
    it "test2" $ do
      parserTest tokenizer "for   = "
        [ Token TokFor 0 3
        , Token TokEquals 6 7
        ]
