-- Renders de Bruijn 'Term's back into readable named syntax for display.
-- Synthesizes fresh variable names (x, y, z, x1, y1, ...) per binder
-- depth so output looks like ordinary lambda calculus rather than indices.
module Pretty
  ( prettyTerm
  , prettyTermDeBruijn
  ) where

import Term (Term (..))

nameSupply :: [String]
nameSupply = [base ++ suffix | suffix <- "" : map show [1 :: Int ..], base <- baseNames]
  where
    baseNames = map (: []) "xyzwuvtsrqp"

prettyTerm :: Term -> String
prettyTerm = go [] False
  where
    go :: [String] -> Bool -> Term -> String
    go scope _ (Var i) = case drop i scope of
      (n : _) -> n
      [] -> "#" ++ show (i - length scope) -- free index escaping all binders (shouldn't happen for closed terms)
    go scope _ (Lam body) =
      let n = freshName scope
       in "\\" ++ n ++ " -> " ++ go (n : scope) False body
    go scope _ (App f a) =
      let f' = goParenLeft scope f
          a' = goParenAtom scope a
       in f' ++ " " ++ a'

    goParenLeft scope t@(Lam _) = "(" ++ go scope False t ++ ")"
    goParenLeft scope t = go scope False t

    goParenAtom scope t@(Var _) = go scope False t
    goParenAtom scope t = "(" ++ go scope False t ++ ")"

    freshName scope = head (filter (`notElem` scope) nameSupply)

prettyTermDeBruijn :: Term -> String
prettyTermDeBruijn (Var i) = show i
prettyTermDeBruijn (Lam body) = "\\." ++ prettyTermDeBruijn body
prettyTermDeBruijn (App f a) = pf ++ " " ++ pa
  where
    pf = case f of
      Lam _ -> "(" ++ prettyTermDeBruijn f ++ ")"
      _ -> prettyTermDeBruijn f
    pa = case a of
      Var _ -> prettyTermDeBruijn a
      _ -> "(" ++ prettyTermDeBruijn a ++ ")"
