
module Pseudolang.ASTBuilderNG where

import Pseudolang.Prelude

import Pseudolang.ParserNG

(@==) :: Expr -> Expr -> Expr
(@==) = ExprEquals

assignStmnt :: Identifier -> Expr -> Statement
assignStmnt ident expr = StatementAssignment (assign ident expr)

assign :: Identifier -> Expr -> Assignment
assign ident expr = (Assignment (AssignmentLHSIdentifier ident) expr)

(@=) :: Identifier -> Expr -> Statement
(@=) = assignStmnt

ifStmnt :: Expr -> NonEmpty Statement -> Statement
ifStmnt expr statements = StatementIf (If expr statements Nothing)


