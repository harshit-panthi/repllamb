-- De Bruijn-indexed lambda calculus terms, plus shifting, substitution,
-- and normal-order beta reduction.
module Term
  ( Term (..)
  , shift
  , subst
  , betaReduceStep
  , termSize
  , StopReason (..)
  , normalize
  , normalizeTrace
  ) where

-- Lambda terms using de Bruijn indices: a 'Var' refers to the binder
-- that is n lambdas out from it (0 = the nearest enclosing lambda).
data Term
  = Var Int          -- de Bruijn index
  | Lam Term         -- binder; body has all indices relative to it
  | App Term Term
  deriving (Eq, Show)

-- Number of AST nodes in a term. Used as a guard against single
-- reduction steps that blow the term up (e.g. self-duplicating
-- combinators), independent of how many steps have been taken.
termSize :: Term -> Int
termSize (Var _)   = 1
termSize (Lam b)   = 1 + termSize b
termSize (App f a) = 1 + termSize f + termSize a

-- shift d c t adds d to every free variable in t that is >= cutoff c.
-- Used when a term is moved under (d > 0) or out of (d < 0) a binder.
shift :: Int -> Int -> Term -> Term
shift d c (Var k)
  | k >= c    = Var (k + d)
  | otherwise = Var k
shift d c (Lam body) = Lam (shift d (c + 1) body)
shift d c (App f a)  = App (shift d c f) (shift d c a)

-- subst j s t replaces free occurrences of Var j in t with s,
-- correctly shifting s as it crosses binders (capture-avoiding by
-- construction, since indices are positional rather than named).
subst :: Int -> Term -> Term -> Term
subst j s (Var k)
  | k == j    = s
  | otherwise = Var k
subst j s (Lam body) = Lam (subst (j + 1) (shift 1 0 s) body)
subst j s (App f a)  = App (subst j s f) (subst j s a)

-- Beta-reduce the outermost redex under a lambda:
-- (\.body) arg  ~>  body[0 := arg], with the result shifted back down
-- by one to account for the removed binder.
applyBeta :: Term -> Term -> Term
applyBeta body arg = shift (-1) 0 (subst 0 (shift 1 0 arg) body)

-- Perform a single normal-order (leftmost-outermost) beta reduction step.
-- Returns 'Nothing' if the term is already in normal form.
betaReduceStep :: Term -> Maybe Term
betaReduceStep (App (Lam body) arg) = Just (applyBeta body arg)
betaReduceStep (App f a) = case betaReduceStep f of
  Just f' -> Just (App f' a)
  Nothing -> App f <$> betaReduceStep a
betaReduceStep (Lam body) = Lam <$> betaReduceStep body
betaReduceStep (Var _) = Nothing

-- Why reduction stopped before (necessarily) returning a normal form.
data StopReason
  = Normalized
  | StepLimitHit
  | SizeLimitHit
  deriving (Eq, Show)

-- Normalize a term, taking at most stepLimit steps and refusing to
-- continue past sizeLimit AST nodes, to guard against both divergence
-- (e.g. the Omega combinator) and single-step blowup (e.g. combinators
-- that duplicate their argument each reduction, like the Y-combinator)
normalize :: Int -> Int -> Term -> (Term, StopReason)
normalize stepLimit sizeLimit = go stepLimit
  where
    go 0 t = (t, StepLimitHit)
    go n t = case betaReduceStep t of
      Nothing -> (t, Normalized)
      Just t'
        | termSize t' > sizeLimit -> (t, SizeLimitHit)
        | otherwise -> go (n - 1) t'

-- Like 'normalize', but also returns every intermediate term, for
-- step-by-step REPL display.
normalizeTrace :: Int -> Int -> Term -> ([Term], StopReason)
normalizeTrace stepLimit sizeLimit t0 = go stepLimit t0 [t0]
  where
    go 0 _ acc = (reverse acc, StepLimitHit)
    go n t acc = case betaReduceStep t of
      Nothing -> (reverse acc, Normalized)
      Just t'
        | termSize t' > sizeLimit -> (reverse acc, SizeLimitHit)
        | otherwise -> go (n - 1) t' (t' : acc)
