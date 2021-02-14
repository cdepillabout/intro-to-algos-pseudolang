{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}

module Test.Pseudolang.ParserTest where

import Pseudolang.Prelude

import Data.String.Interpolate (__i)
import Test.Hspec (Spec, describe, expectationFailure, fit, it, shouldBe)
import Text.Megaparsec (eof, parse)
import Text.Pretty.Simple (pShow)

import Pseudolang.ASTBuilder
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
          expectedAST = ExprPlus (ExprVar "foo") (ExprVar "bar")
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
              (AssignmentLHSIdentifier "x")
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
                (AssignmentLHSIdentifier "x")
                (ExprPlus (ExprInteger 1) (ExprInteger 2))
              )
      parserTest statementParser input expectedAST
    it "test7" $ do
      let input = "x = 1 + 2"
          expectedAST =
            [ StatementAssignment
                (Assignment
                  (AssignmentLHSIdentifier "x")
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
                      (AssignmentLHSIdentifier "x")
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
                           (AssignmentLHSIdentifier (Identifier "x"))
                           (ExprInteger 3))
                        ForDirectionTo
                        (ExprInteger 7)
                        [ StatementAssignment
                            (Assignment
                               (AssignmentLHSIdentifier (Identifier "a"))
                               (ExprInteger 4))
                        ]))
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
                  (StatementAssignment
                     (Assignment
                        (AssignmentLHSIdentifier (Identifier "a"))
                        (ExprInteger 4)))
              , TopLevelStatement
                  (StatementAssignment
                     (Assignment
                        (AssignmentLHSIdentifier (Identifier "b"))
                        (ExprInteger 3)))
              ]
      parserTest astParser input expectedAST
    it "test11" $ do
      let input =
            [__i|
              for x = 3 to 7
                b = 3
             |]
          expectedAST =
            ForLoop
              (Assignment (AssignmentLHSIdentifier (Identifier "x")) (ExprInteger 3))
              ForDirectionTo
              (ExprInteger 7)
              [ StatementAssignment
                  (Assignment (AssignmentLHSIdentifier (Identifier "b")) (ExprInteger 3))
              ]
      parserTest forParser input expectedAST
    it "test12" $ do
      let input = "  a = 4\n  b = 3"
          expectedAST =
            [ StatementAssignment
                (Assignment (AssignmentLHSIdentifier (Identifier "a")) (ExprInteger 4))
            , StatementAssignment
                (Assignment (AssignmentLHSIdentifier (Identifier "b")) (ExprInteger 3))
            ]
      parserTestWithIndent 2 statementsParser input expectedAST
    it "test13" $ do
      let input = "a = 4"
          expectedAST =
            StatementAssignment
              (Assignment (AssignmentLHSIdentifier (Identifier "a")) (ExprInteger 4))
      parserTestWithIndent 2 statementAssignmentParser input expectedAST
    it "test14" $ do
      let input = "  a = 4"
          expectedAST =
            StatementAssignment
              (Assignment (AssignmentLHSIdentifier (Identifier "a")) (ExprInteger 4))
      parserTestWithIndent 2 indentedStatementParser input expectedAST
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
                  (FunDef
                     (Identifier "Insertion-Sort")
                     [Identifier "a"]
                     [ StatementAssignment
                         (Assignment
                            (AssignmentLHSIdentifier (Identifier "a"))
                            (ExprInteger 10))
                     ])
              , TopLevelStatement
                  (StatementAssignment
                     (Assignment
                        (AssignmentLHSIdentifier (Identifier "b"))
                        (ExprInteger 20)))
              ]
      parserTest astParser input expectedAST
    it "test16" $ do
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
                     [ StatementAssignment
                         (Assignment
                            (AssignmentLHSIdentifier (Identifier "a"))
                            (ExprInteger 10))
                     ])
              , TopLevelStatement
                  (StatementAssignment
                     (Assignment
                        (AssignmentLHSIdentifier (Identifier "b"))
                        (ExprInteger 20)))
              , TopLevelStatement
                  (StatementAssignment
                     (Assignment
                        (AssignmentLHSIdentifier (Identifier "c"))
                        (ExprFunCall
                           (FunCall
                              (Identifier "Insertion-Sort")
                              (Tuple
                                [ ExprVar (Identifier "b")
                                , ExprPlus (ExprInteger 1) (ExprInteger 2)
                                ])))))
              , TopLevelStatement
                  (StatementFunCall
                      (FunCall (Identifier "print") (Tuple [ExprInteger 5])))
              , TopLevelStatement
                  (StatementFunCall (FunCall (Identifier "goto") (Tuple [])))
              ]
      parserTest astParser input expectedAST
    it "test17" $ do
      let input = "[ 1, 3+4, 5 ]"
          expectedAST =
            ExprArrayLit
              [ExprInteger 1, ExprPlus (ExprInteger 3) (ExprInteger 4), ExprInteger 5]
      parserTestWithIndent 2 exprParser input expectedAST
    it "test18" $ do
      let input =
            [__i|
              while x < 10
                b = 3
             |]
          expectedAST =
            WhileLoop
              (ExprLessThan (ExprVar (Identifier "x")) (ExprInteger 10))
              [ StatementAssignment
                  (Assignment (AssignmentLHSIdentifier (Identifier "b")) (ExprInteger 3))
              ]
      parserTest whileParser input expectedAST
    it "test19" $ do
      let input = "[ 1, \"x y\", 5 ]"
          expectedAST =
            ExprArrayLit
              [ExprInteger 1, ExprString "x y", ExprInteger 5]
      parserTestWithIndent 2 exprParser input expectedAST
    it "test20" $ do
      let input =
            [__i|
              if k < 3
                b = 3
             |]
          expectedAST =
            If
              (ExprLessThan (ExprVar (Identifier "k")) (ExprInteger 3))
              [ StatementAssignment
                  (Assignment (AssignmentLHSIdentifier (Identifier "b")) (ExprInteger 3))
              ]
              Nothing
      parserTest ifParser input expectedAST
    it "test21" $ do
      let input =
            [__i|
              if k < 3
                b = 3
              else c = 3
             |]
          expectedAST =
            If
              (ExprLessThan (ExprVar "k") (ExprInteger 3))
              [ StatementAssignment
                  (Assignment (AssignmentLHSIdentifier "b") (ExprInteger 3))
              ]
              (Just
                (ElseIfElse
                  []
                  [ StatementAssignment
                      (Assignment
                        (AssignmentLHSIdentifier "c") (ExprInteger 3))
                  ]
                )
              )
      parserTest ifParser input expectedAST
    it "test22" $ do
      let input =
            [__i|
              if k < 3
                b = 3
              else c = 3
                d = 4
             |]
          expectedAST =
            If
              (ExprLessThan (ExprVar "k") (ExprInteger 3))
              [ StatementAssignment
                  (Assignment (AssignmentLHSIdentifier (Identifier "b")) (ExprInteger 3))
              ]
              (Just
                (ElseIfElse
                  []
                  [ StatementAssignment
                      (Assignment
                        (AssignmentLHSIdentifier "c") (ExprInteger 3))
                  , StatementAssignment
                      (Assignment
                        (AssignmentLHSIdentifier "d") (ExprInteger 4))
                  ]
                )
              )
      parserTest ifParser input expectedAST
    it "test23" $ do
      let input =
            [__i|
              if k < 3
                b = 3
              elseif v > 10
                x = 4
              else c = 3
                d = 4
             |]
          expectedAST =
            If
              (ExprLessThan (ExprVar "k") (ExprInteger 3))
              [ StatementAssignment
                  (Assignment (AssignmentLHSIdentifier "b") (ExprInteger 3))
              ]
              (Just
                (ElseIfElse
                  [ ElseIf
                      (ExprGreaterThan (ExprVar "v") (ExprInteger 10))
                      [ StatementAssignment
                          (Assignment
                            (AssignmentLHSIdentifier "x") (ExprInteger 4))
                      ]
                  ]
                  [ StatementAssignment
                      (Assignment
                        (AssignmentLHSIdentifier "c") (ExprInteger 3))
                  , StatementAssignment
                      (Assignment
                        (AssignmentLHSIdentifier "d") (ExprInteger 4))
                  ]
                )
              )
      parserTest ifParser input expectedAST
    it "test24" $ do
      let input =
            [__i|
              elseif v > 10
                x = 4
                b = 10
             |]
          expectedAST =
            ElseIf
              (ExprGreaterThan (ExprVar "v") (ExprInteger 10))
              [ StatementAssignment
                  (Assignment
                    (AssignmentLHSIdentifier "x") (ExprInteger 4))
              , StatementAssignment
                  (Assignment
                    (AssignmentLHSIdentifier "b") (ExprInteger 10))
              ]
      parserTest elseIfParser input expectedAST
    it "test25" $ do
      let input =
            [__i|
              x = infinity
             |]
          expectedAST =
            StatementAssignment
              (Assignment
                (AssignmentLHSIdentifier "x") ExprInfinity)
      parserTest statementParser input expectedAST
    it "test26" $ do
      let input =
            [__i|
              if k < 3
                b = 3
                d = 4
              else c = 3
                d = 4
             |]
          expectedAST =
            If
              (ExprLessThan (ExprVar "k") (ExprInteger 3))
              [ StatementAssignment
                  (Assignment (AssignmentLHSIdentifier "b") (ExprInteger 3))
              , StatementAssignment
                  (Assignment (AssignmentLHSIdentifier "d") (ExprInteger 4))
              ]
              (Just
                (ElseIfElse
                  []
                  [ StatementAssignment
                      (Assignment
                        (AssignmentLHSIdentifier "c") (ExprInteger 3))
                  , StatementAssignment
                      (Assignment
                        (AssignmentLHSIdentifier "d") (ExprInteger 4))
                  ]
                )
              )
      parserTest ifParser input expectedAST
    it "test27" $ do
      let input =
            [__i|
              if k < 3
                b = 3
                d = 4
              else c = 3
                d = 4
             |]
          expectedAST =
            StatementIf
              ( If
                  (ExprLessThan (ExprVar "k") (ExprInteger 3))
                  [ StatementAssignment
                      (Assignment (AssignmentLHSIdentifier "b") (ExprInteger 3))
                  , StatementAssignment
                      (Assignment (AssignmentLHSIdentifier "d") (ExprInteger 4))
                  ]
                  (Just
                    (ElseIfElse
                      []
                      [ StatementAssignment
                          (Assignment
                            (AssignmentLHSIdentifier "c") (ExprInteger 3))
                      , StatementAssignment
                          (Assignment
                            (AssignmentLHSIdentifier "d") (ExprInteger 4))
                      ]
                    )
                  )
              )
      parserTest statementParser input expectedAST
    it "test28" $ do
      let input =
            [__i|
              if k < 3
                b = 3
                d = 4
              else c = 3
                d = 4
             |]
          expectedAST =
            AST
              [ TopLevelStatement
                  ( StatementIf
                      ( If
                          (ExprLessThan (ExprVar "k") (ExprInteger 3))
                          [ StatementAssignment
                              (Assignment (AssignmentLHSIdentifier "b") (ExprInteger 3))
                          , StatementAssignment
                              (Assignment (AssignmentLHSIdentifier "d") (ExprInteger 4))
                          ]
                          (Just
                            (ElseIfElse
                              []
                              [ StatementAssignment
                                  (Assignment
                                    (AssignmentLHSIdentifier "c") (ExprInteger 3))
                              , StatementAssignment
                                  (Assignment
                                    (AssignmentLHSIdentifier "d") (ExprInteger 4))
                              ]
                            )
                          )
                      )
                  )
              ]
      parserTest astParser input expectedAST
    it "test29" $ do
      let input =
            [__i|
              if k < 3
                b = 3
                d = 4
              else c = 3
                d = 4
             |]
          expectedAST =
            AST
              [ TopLevelStatement
                  ( StatementIf
                      ( If
                          (ExprLessThan (ExprVar "k") (ExprInteger 3))
                          [ StatementAssignment
                              (Assignment (AssignmentLHSIdentifier "b") (ExprInteger 3))
                          , StatementAssignment
                              (Assignment (AssignmentLHSIdentifier "d") (ExprInteger 4))
                          ]
                          (Just
                            (ElseIfElse
                              []
                              [ StatementAssignment
                                  (Assignment
                                    (AssignmentLHSIdentifier "c") (ExprInteger 3))
                              , StatementAssignment
                                  (Assignment
                                    (AssignmentLHSIdentifier "d") (ExprInteger 4))
                              ]
                            )
                          )
                      )
                  )
              ]
      parserTest astParser input expectedAST
    it "test30" $ do
      let input =
            [__i|
              for k = p to r
                if x <= y
                  g = 3
                else b = 10
                  h = q
             |]
          expectedAST =
            AST
              [ TopLevelStatement
                  ( StatementForLoop
                      ( ForLoop
                          ( Assignment
                            (AssignmentLHSIdentifier "k")
                            (ExprVar "p")
                          )
                          ForDirectionTo
                          (ExprVar "r")
                          [ StatementIf
                              ( If
                                  (ExprLessThanOrEqualTo (ExprVar "x") (ExprVar "y"))
                                  [ StatementAssignment
                                      ( Assignment
                                          (AssignmentLHSIdentifier "g")
                                          (ExprInteger 3)
                                      )
                                  ]
                                  (Just
                                    (ElseIfElse
                                      []
                                      [ StatementAssignment
                                          ( Assignment
                                              (AssignmentLHSIdentifier "b")
                                              (ExprInteger 10)
                                          )
                                      , StatementAssignment
                                          ( Assignment
                                              (AssignmentLHSIdentifier "h")
                                              (ExprVar "q")
                                          )
                                      ]
                                    )
                                  )
                              )
                          ]
                      )
                  )
              ]
      parserTest astParser input expectedAST
    it "test31" $ do
      let input =
            [__i|
              for k = p to r
                if x <= y
                  g = 3
                else b = 10
                  h = q
             |]
          expectedAST =
            ForLoop
              ( Assignment
                (AssignmentLHSIdentifier "k")
                (ExprVar "p")
              )
              ForDirectionTo
              (ExprVar "r")
              [ StatementIf
                  ( If
                      (ExprLessThanOrEqualTo (ExprVar "x") (ExprVar "y"))
                      [ StatementAssignment
                          ( Assignment
                              (AssignmentLHSIdentifier "g")
                              (ExprInteger 3)
                          )
                      ]
                      (Just
                        (ElseIfElse
                          []
                          [ StatementAssignment
                              ( Assignment
                                  (AssignmentLHSIdentifier "b")
                                  (ExprInteger 10)
                              )
                          , StatementAssignment
                              ( Assignment
                                  (AssignmentLHSIdentifier "h")
                                  (ExprVar "q")
                              )
                          ]
                        )
                      )
                  )
              ]
      parserTest forParser input expectedAST
    it "test32" $ do
      let input =
            [__i|
              if x <= y
                g = 3
              else b = 10
                h = q
             |]
          expectedAST =
            StatementIf
              ( If
                  (ExprLessThanOrEqualTo (ExprVar "x") (ExprVar "y"))
                  [ StatementAssignment
                      ( Assignment
                          (AssignmentLHSIdentifier "g")
                          (ExprInteger 3)
                      )
                  ]
                  (Just
                    (ElseIfElse
                      []
                      [ StatementAssignment
                          ( Assignment
                              (AssignmentLHSIdentifier "b")
                              (ExprInteger 10)
                          )
                      , StatementAssignment
                          ( Assignment
                              (AssignmentLHSIdentifier "h")
                              (ExprVar "q")
                          )
                      ]
                    )
                  )
              )
      parserTest statementParser input expectedAST
    it "test33" $ do
      let input =
            [__i|
              if x <= y
                g = 3
              else b = 10
                h = q
             |]
          expectedAST =
            [ StatementIf
                ( If
                    (ExprLessThanOrEqualTo (ExprVar "x") (ExprVar "y"))
                    [ StatementAssignment
                        ( Assignment
                            (AssignmentLHSIdentifier "g")
                            (ExprInteger 3)
                        )
                    ]
                    (Just
                      (ElseIfElse
                        []
                        [ StatementAssignment
                            ( Assignment
                                (AssignmentLHSIdentifier "b")
                                (ExprInteger 10)
                            )
                        , StatementAssignment
                            ( Assignment
                                (AssignmentLHSIdentifier "h")
                                (ExprVar "q")
                            )
                        ]
                      )
                    )
                )
            ]
      parserTest statementsParser input expectedAST
    it "test34" $ do
      let input = "if x <= y\n    g = 3\n  else b = 10\n    h = q\n"
          expectedAST =
            If
              (ExprLessThanOrEqualTo (ExprVar "x") (ExprVar "y"))
              [ StatementAssignment
                  ( Assignment
                      (AssignmentLHSIdentifier "g")
                      (ExprInteger 3)
                  )
              ]
              (Just
                (ElseIfElse
                  []
                  [ StatementAssignment
                      ( Assignment
                          (AssignmentLHSIdentifier "b")
                          (ExprInteger 10)
                      )
                  , StatementAssignment
                      ( Assignment
                          (AssignmentLHSIdentifier "h")
                          (ExprVar "q")
                      )
                  ]
                )
              )
      parserTestWithIndent 2 ifParser input expectedAST
    it "test35" $ do
      let input = "( 1, 3+4, 5 )"
          expectedAST =
            ExprTuple
              (Tuple
                [ ExprInteger 1
                , ExprPlus (ExprInteger 3) (ExprInteger 4)
                , ExprInteger 5
                ]
              )
      parserTest exprParser input expectedAST
    it "test36" $ do
      let input = "( 1 )"
          expectedAST = ExprInteger 1
      parserTest exprParser input expectedAST
    it "test37" $ do
      let input = "( )"
          expectedAST = ExprTuple (Tuple [])
      parserTest exprParser input expectedAST
    it "test38" $ do
      let input = "(x, y) = (1, 2)"
          expectedAST =
            Assignment
              (AssignmentLHSTuple
                [ AssignmentLHSIdentifier "x"
                , AssignmentLHSIdentifier "y"
                ]
              )
              (ExprTuple
                (Tuple
                  [ ExprInteger 1
                  , ExprInteger 2
                  ]
                )
              )
      parserTest assignmentParser input expectedAST
    it "test39" $ do
      let input = "(x, (y, A[3]), z) = (1, (2, 7), 9)"
          expectedAST =
            Assignment
              (AssignmentLHSTuple
                [ AssignmentLHSIdentifier "x"
                , AssignmentLHSTuple
                    [ AssignmentLHSIdentifier "y"
                    , AssignmentLHSArrayIndex (ArrayIndex "A" (ExprInteger 3))
                    ]
                , AssignmentLHSIdentifier "z"
                ]
              )
              (ExprTuple
                (Tuple
                  [ ExprInteger 1
                  , ExprTuple (Tuple [ExprInteger 2, ExprInteger 7])
                  , ExprInteger 9
                  ]
                )
              )
      parserTest assignmentParser input expectedAST
    it "test40" $ do
      let input = "A.heap-size = 3"
          expectedAST =
            Assignment
              (AssignmentLHSProperty (Property "A" "heap-size"))
              (ExprInteger 3)
      parserTest assignmentParser input expectedAST
    -- it "test41" $ do
    --   let input =
    --         [__i|
    --           fun X()
    --             if 1 == 1
    --               largest = 11
    --             if 6 == 11
    --               largest = 100
    --          |]
    --       expectedAST =
    --         FunDef
    --           "X"
    --           []
    --           [ ifStmnt (1 @== 1) [ assignStmnt "largest" 11 ]
    --           , ifStmnt (6 @== 11) [ assignStmnt "largest" 100 ]
    --           ]
    --   parserTest funDefParser input expectedAST
    -- it "test42" $ do
    --   let input =
    --         [__i|
    --           if 1 == 1
    --             largest = 11
    --           if 6 == 11
    --             largest = 100
    --          |]
    --       expectedAST =
    --         [ ifStmnt (1 @== 1) [ assignStmnt "largest" 11 ]
    --         , ifStmnt (6 @== 11) [ assignStmnt "largest" 100 ]
    --         ]
    --   parserTest statementsParser input expectedAST
    -- it "test43" $ do
    --   let input =
    --         [__i|
    --           if 1 == 1
    --             if 3 == 3
    --               largest = 11
    --             if 6 == 11
    --               largest = 100
    --          |]
    --       expectedAST =
    --         [ ifStmnt
    --             (1 @== 1)
    --             [ ifStmnt (3 @== 3) [ assignStmnt "largest" 11 ]
    --             , ifStmnt (6 @== 11) [ assignStmnt "largest" 100 ]
    --             ]
    --         ]
    --   parserTest statementsParser input expectedAST
    -- it "test44" $ do
    --   let input =
    --         [__i|
    --           if 1 == 1
    --             if 3 == 3
    --               largest = 11
    --             x = 10
    --          |]
    --       expectedAST =
    --         [ ifStmnt
    --             (1 @== 1)
    --             [ ifStmnt (3 @== 3) [ assignStmnt "largest" 11 ]
    --             , assignStmnt "x" 10
    --             ]
    --         ]
    --   parserTest statementsParser input expectedAST
    fit "test45" $ do
      let input = "  if 1 == 2\n    x = 3\n  y = 4\n"
          expectedAST =
            [ ifStmnt (1 @== 2) [ assignStmnt "x" 3 ]
            , assignStmnt "y" 4
            ]
      parserTest (indented statementsParser) input expectedAST
