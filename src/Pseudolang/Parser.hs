
module Pseudolang.Parser where

import Pseudolang.Prelude hiding (many, some, try)

import Text.Megaparsec (ParsecT, Pos, SourcePos, choice, getOffset, many, mkPos, notFollowedBy, some, try)
import Text.Megaparsec.Char (alphaNumChar, char, hspace1, letterChar, string)


newtype AST = AST { unAST :: StatementList }

newtype StatementList = StatementList { unStatementList :: NonEmpty Statement }

data Statement
  = StatementAssignment Assignment
  | StatementForLoop Assignment ForDirection Expr StatementList
  | StatementIf Expr StatementList
  | StatementReturn Expr
  | StatementFuncDef Identifier [Identifier] StatementList

data Assignment = Assignment Identifier Expr

data ForDirection = DownTo | To
