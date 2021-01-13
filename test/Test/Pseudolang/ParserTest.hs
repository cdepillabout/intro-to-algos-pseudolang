
module Test.Pseudolang.ParserTest where

import Pseudolang.Prelude

import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Text.Megaparsec (eof, errorBundlePretty, mkPos, parse)
import Text.Pretty.Simple (pShow)

import Pseudolang.Lexer (Tok(..), Token(Token), tokenizer)
import Pseudolang.Parser
import qualified Test.Pseudolang.LexerTest as Test.Lexer

parserFromTokenizerTest ::
     ( Eq a
     , Show a
     -- , VisualStream [Token]
     )
  => Parser a
  -> [Token]
  -> a
  -> IO ()
parserFromTokenizerTest readerTParser input expectedRes = do
  let initialIndentAmount = 0
      parser = runReaderT readerTParser initialIndentAmount
      eitherRes = parse (parser <* eof) "" input
  case eitherRes of
    Right res -> res `shouldBe` expectedRes
    Left err -> expectationFailure $ unpack $ pShow err

parserTest ::
     ( Eq a
     , Show a
     -- , VisualStream [Token]
     )
  => Parser a
  -> Text
  -> a
  -> IO ()
parserTest readerTParser input expectedRes = do
  lexerOutput <- Test.Lexer.parserTest' tokenizer input
  let initialIndentAmount = 0
      parser = runReaderT readerTParser initialIndentAmount
      eitherRes = parse (parser <* eof) "" lexerOutput
  case eitherRes of
    Right res -> res `shouldBe` expectedRes
    Left err -> expectationFailure $ unpack $ pShow err

mkTok :: Tok -> Token
mkTok tok = Token tok 0 0

mkToks :: [Tok] -> [Token]
mkToks = fmap mkTok

test :: Spec
test =
  describe "Parser" $ do
    it "test1" $ do
      parserFromTokenizerTest identParser [mkTok (TokIdentifier "foo")] (Identifier "foo")
    it "test2" $ do
      let inputTokens =
            [ TokIdentifier "foo"
            , TokPlus
            , TokIdentifier "bar"
            ]
          expectedAST = ExprPlus (ExprVar (Identifier "foo")) (ExprVar (Identifier "bar"))
      parserFromTokenizerTest exprParser (mkToks inputTokens) expectedAST
    it "test3" $ do
      let inputTokens =
            [ TokInteger 3
            , TokPlus
            , TokInteger 4
            , TokTimes
            , TokInteger 5
            ]
          expectedAST =
            ExprPlus
              (ExprInteger 3)
              (ExprTimes
                (ExprInteger 4)
                (ExprInteger 5)
              )
      parserFromTokenizerTest exprParser (mkToks inputTokens) expectedAST
    it "test4" $ do
      let input = "x = 1 + 2"
          expectedAST =
            Assignment
              (Identifier "x")
              (ExprPlus
                (ExprInteger 1)
                (ExprInteger 2)
              )
      parserTest assignmentParser input expectedAST
    it "test5" $ do
      let input = "1 + 2"
          expectedAST =
            (ExprPlus
                (ExprInteger 1)
                (ExprInteger 2)
            )
      parserTest exprParser input expectedAST
    it "test6" $ do
      let input = "x = 1 + 2"
          expectedAST =
            StatementAssignment
              (Assignment
                (Identifier "x")
                (ExprPlus (ExprInteger 1) (ExprInteger 2))
              )
      parserTest statementParser input expectedAST
    it "test7" $ do
      let input = "x = 1 + 2"
          expectedAST =
            [ StatementAssignment
                (Assignment
                  (Identifier "x")
                  (ExprPlus (ExprInteger 1) (ExprInteger 2))
                )
            ]
      parserTest statementsParser input expectedAST
    it "test8" $ do
      let input = "x = 1 + 2"
          expectedAST =
            AST
              [ TopLevelStatement
                  (StatementAssignment
                    (Assignment
                      (Identifier "x")
                      (ExprPlus (ExprInteger 1) (ExprInteger 2))
                    )
                  )
              ]
      parserTest astParser input expectedAST
