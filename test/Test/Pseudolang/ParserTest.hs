{-# LANGUAGE QuasiQuotes #-}

module Test.Pseudolang.ParserTest where

import Pseudolang.Prelude

import Data.String.Interpolate (__i)
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
  => Parser a -> Text -> a -> IO ()
parserTest = parserTestWithIndent 0

parserTestWithIndent ::
     ( Eq a
     , Show a
     -- , VisualStream [Token]
     )
  => Int -- ^ Initial indent amount
  -> Parser a
  -> Text
  -> a
  -> IO ()
parserTestWithIndent initialIndentAmount readerTParser input expectedRes = do
  lexerOutput <- Test.Lexer.parserTest' tokenizer input
  let parser = runReaderT readerTParser initialIndentAmount
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
    it "test9" $ do
      let input =
            [__i|
              for x = 3 to 7
                a = 4
             |]
          expectedAST =
            AST
              [ TopLevelStatement
                  (StatementForLoop
                    (ForLoop
                      (Assignment
                        (Identifier "x")
                        (ExprInteger 3))
                      ForDirectionTo
                      (ExprInteger 7)
                      [ StatementAssignment
                          (Assignment (Identifier "a") (ExprInteger 4))
                      ]
                    )
                  )
              ]
      parserTest astParser input expectedAST
    it "test10" $ do
      let input =
            [__i|
              a = 4
              b = 3
             |]
          expectedAST =
            AST
              [ TopLevelStatement
                  (StatementAssignment (Assignment (Identifier "a") (ExprInteger 4)))
              , TopLevelStatement
                  (StatementAssignment (Assignment (Identifier "b") (ExprInteger 3)))
              ]
      parserTest astParser input expectedAST
    it "test11" $ do
      let input =
            [__i|
              for x = 3 to 7
                b = 3
             |]
          expectedAST =
            ForLoop (Assignment (Identifier "x") (ExprInteger 3)) ForDirectionTo (ExprInteger 7)
              [ StatementAssignment (Assignment (Identifier "b") (ExprInteger 3))
              ]
      parserTest forParser input expectedAST
    it "test12" $ do
      let input = "  a = 4\n  b = 3"
          expectedAST =
            [ StatementAssignment (Assignment (Identifier "a") (ExprInteger 4))
            , StatementAssignment (Assignment (Identifier "b") (ExprInteger 3))
            ]
      parserTestWithIndent 2 statementsParser input expectedAST
    it "test13" $ do
      let input = "a = 4"
          expectedAST = StatementAssignment (Assignment (Identifier "a") (ExprInteger 4))
      parserTestWithIndent 2 statementAssignmentParser input expectedAST
    it "test14" $ do
      let input = "  a = 4"
          expectedAST = StatementAssignment (Assignment (Identifier "a") (ExprInteger 4))
      parserTestWithIndent 2 statementParser input expectedAST
    it "test15" $ do
      let input =
            [__i|
              fun Insertion-Sort(a)
                a = 10
              b = 20
             |]
          expectedAST =
            AST
              [ TopLevelFunDef
                  (FunDef (Identifier "Insertion-Sort") [Identifier "a"]
                    [ StatementAssignment (Assignment (Identifier "a") (ExprInteger 10))
                    ]
                  )
              , TopLevelStatement
                  (StatementAssignment (Assignment (Identifier "b") (ExprInteger 20)))
              ]
      parserTest astParser input expectedAST
    it "test15" $ do
      let input =
            [__i|
              fun Insertion-Sort(a, x)
                a = 10
              b = 20
              c = Insertion-Sort(b, 1 + 2)
              print(5)
              goto()
             |]
          expectedAST =
            AST
              [ TopLevelFunDef
                  (FunDef
                     (Identifier "Insertion-Sort")
                     [Identifier "a", Identifier "x"]
                     [StatementAssignment (Assignment (Identifier "a") (ExprInteger 10))])
              , TopLevelStatement
                  (StatementAssignment (Assignment (Identifier "b") (ExprInteger 20)))
              , TopLevelStatement
                  (StatementAssignment
                     (Assignment
                        (Identifier "c")
                        (ExprFunCall
                           (FunCall
                              (Identifier "Insertion-Sort")
                              [ ExprVar (Identifier "b")
                              , ExprPlus (ExprInteger 1) (ExprInteger 2)
                              ]
                           )
                        )
                     )
                  )
              , TopLevelStatement
                  (StatementFunCall (FunCall (Identifier "print") [ExprInteger 5]))
              , TopLevelStatement
                  (StatementFunCall (FunCall (Identifier "goto") []))
              ]
      parserTest astParser input expectedAST
