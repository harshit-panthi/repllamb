module Compile
  ( CompileError (..)
  , compile
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Syntax (Expr (..))
import Term (Term (..))

data CompileError
  = UnboundVariable String
  | ShadowedBinder String
  deriving (Eq)

instance Show CompileError where
  show (UnboundVariable name) = "unbound variable: " ++ name
  show (ShadowedBinder name) =
    "lambda binder shadows existing binding: " ++ name
      ++ " (rename the parameter, or rebind " ++ name ++ " with `let` instead)"

-- Compile a surface expression to a de Bruijn term.
compile :: Map String Term -> Expr -> Either CompileError Term
compile env = go []
  where
    go :: [String] -> Expr -> Either CompileError Term
    go scope (EVar x) = case lookup x (zip scope [0 ..]) of
      Just i -> Right (Var i)
      Nothing -> case Map.lookup x env of
        Just t -> Right t
        Nothing -> Left (UnboundVariable x)
    go scope (ELam [] body) = go scope body
    go scope (ELam (n : ns) body)
      | n `Map.member` env = Left (ShadowedBinder n)
      | otherwise = Lam <$> go (n : scope) (ELam ns body)
    go scope (EApp f a) = App <$> go scope f <*> go scope a
