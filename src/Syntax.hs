{-# LANGUAGE OverloadedStrings #-}

-- Surface syntax for the REPL: ordinary named-variable lambda terms,
-- parsed with Megaparsec. These get compiled to de Bruijn 'Term's
module Syntax
  ( Expr (..)
  , Stmt (..)
  , parseStmt
  , parseExprOnly
  ) where

import Control.Monad (void)
import Data.Void (Void)
import Data.Char (isAlpha, isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Expr
  = EVar String
  | ELam [String] Expr
  | EApp Expr Expr
  deriving (Eq, Show)

data Stmt
  = SLet String Expr
  | SExpr Expr
  deriving (Eq, Show)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reservedWords :: [String]
reservedWords = ["let", "in"]

identifier :: Parser String
identifier = lexeme . try $ do
  c0 <- satisfy (\c -> isAlpha c || c == '_')
  cs <- many (satisfy (\c -> isAlphaNum c || c == '_' || c == '\''))
  let name = c0 : cs
  if name `elem` reservedWords
    then fail (name ++ " is a reserved word")
    else pure name

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Lambda binder: either backslash or actual lambda character.
lambdaSym :: Parser ()
lambdaSym = void (symbol "\\") <|> void (symbol "\955") -- 'λ'

-- The arrow or dot separating the binder list from the body.
arrowSym :: Parser ()
arrowSym = void (symbol "->") <|> void (symbol ".")

pVar :: Parser Expr
pVar = EVar <$> identifier

pLam :: Parser Expr
pLam = do
  lambdaSym
  names <- some identifier
  arrowSym
  body <- pExpr
  pure (ELam names body)

pAtom :: Parser Expr
pAtom = pVar <|> parens pExpr

-- Application is juxtaposition, left-associative: f a b = (f a) b.
pApp :: Parser Expr
pApp = foldl1 EApp <$> some pAtom

pExpr :: Parser Expr
pExpr = pLam <|> pApp

pLet :: Parser Stmt
pLet = do
  _ <- symbol "let"
  name <- identifier
  _ <- symbol "="
  e <- pExpr
  pure (SLet name e)

pStmt :: Parser Stmt
pStmt = try pLet <|> (SExpr <$> pExpr)

-- Parse one full REPL line (consuming leading/trailing whitespace).
parseStmt :: String -> Either String Stmt
parseStmt input = case parse (sc *> pStmt <* eof) "<repl>" input of
  Left err -> Left (errorBundlePretty err)
  Right s -> Right s

-- Parse a bare expression (used for tests / non-REPL entry points).
parseExprOnly :: String -> Either String Expr
parseExprOnly input = case parse (sc *> pExpr <* eof) "<expr>" input of
  Left err -> Left (errorBundlePretty err)
  Right e -> Right e
