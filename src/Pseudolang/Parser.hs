
module Pseudolang.Parser where

import Pseudolang.Prelude hiding (many, some, try)

import qualified Data.Set as Set
import Text.Megaparsec (ErrorItem, ParsecT, Pos, SourcePos, choice, getOffset, many, mkPos, notFollowedBy, some, token, try)
import Text.Megaparsec.Char (alphaNumChar, char, hspace1, letterChar, string)

import Pseudolang.Lexer (Tok(..), Token(Token))

type Parser = ParsecT Void [Token] Identity

newtype AST = AST { unAST :: TopLevelList }
  deriving stock (Eq, Ord, Show)

newtype TopLevelList = TopLevelList
  { unTopLevelList :: NonEmpty (Either Statement FuncDef)
  }
  deriving stock (Eq, Ord, Show)

data TopLevel = TopLevelStatement Statement | TopLevelFuncDef FuncDef
  deriving stock (Eq, Ord, Show)

data Statement
  = StatementAssignment Assignment
  | StatementForLoop Assignment ForDirection Expr [Statement]
  | StatementIf Expr [Statement]
  | StatementReturn Expr
  deriving stock (Eq, Ord, Show)

data FuncDef = FuncDef Identifier [Identifier] [Statement]
  deriving stock (Eq, Ord, Show)

data Assignment = Assignment Identifier Expr
  deriving stock (Eq, Ord, Show)

data ForDirection = DownTo | To
  deriving stock (Eq, Ord, Show)

data Expr = Expr
  deriving stock (Eq, Ord, Show)

newtype Identifier = Identifier Text
  deriving stock (Eq, Ord, Show)

identParser :: Parser Identifier
identParser = do
  ident <- token f Set.empty
  pure $ Identifier ident
  where
    f :: Token -> Maybe Text
    f (Token (TokIdentifier ident) _ _) = Just ident
    f _ = Nothing

-- assignmentParser :: Parser Assignment
-- assignmentParser = do
--   ident <- token 
