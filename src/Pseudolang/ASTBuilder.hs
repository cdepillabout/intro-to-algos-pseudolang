
module Pseudolang.ASTBuilder where

import Pseudolang.Prelude

import Pseudolang.Parser

(@==) :: Expr -> Expr -> Expr
(@==) = ExprEquals

assignStmnt :: Identifier -> Expr -> Statement
assignStmnt ident expr =
  StatementAssignment (Assignment (AssignmentLHSIdentifier ident) expr)

ifStmnt :: Expr -> NonEmpty Statement -> Statement
ifStmnt expr statements = StatementIf (If expr statements Nothing)

