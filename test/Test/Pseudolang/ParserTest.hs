
module Test.Pseudolang.ParserTest where

import Pseudolang.Prelude

import Text.Megaparsec (eof, errorBundlePretty, mkPos, parse)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Pseudolang.Lexer (Tok(..), Token(Token))
import Pseudolang.Parser

parserTest ::
     ( Eq a
     , Show a
     -- , VisualStream [Token]
     )
  => Parser a
  -> [Token]
  -> a
  -> IO ()
parserTest parser input expectedRes = do
  let eitherRes = parse (parser <* eof) "" input
  case eitherRes of
    Right res -> res `shouldBe` expectedRes
    Left err -> expectationFailure $ show err

mkTok :: Tok -> Token
mkTok tok = Token tok 0 0

test :: Spec
test =
  describe "Parser" $ do
    it "test1" $ do
      parserTest identParser [mkTok (TokIdentifier "foo")] (Identifier "foo")
