# lambda-repl

An untyped lambda calculus REPL in Haskell, using de Bruijn indices
internally, with an environment-based `let`.

## Build

```
cabal build
```

## Architecture

- `Term.hs` — the core de Bruijn-indexed `Term` type (`Var Int | Lam Term | App Term Term`),
  with `shift`/`subst`, normal-order beta reduction (`betaReduceStep`), `termSize`
  (AST node count), and `normalize`/`normalizeTrace`, which reduce a term subject to
  both a step-count limit and a size-cap (see "Reduction limits" below).
- `Syntax.hs` — Megaparsec parser for the surface syntax: named-variable lambdas
  (`\x -> e`, `\x y -> e`, or `λx. e`), application by juxtaposition, and
  `let name = expr` statements.
- `Compile.hs` — compiles named `Expr` to de Bruijn `Term`, resolving lexically
  bound variables to indices and free variables against an environment of
  previously `let`-bound (closed) terms. Rejects expressions with free variables
  (`UnboundVariable`) and lambda parameters that shadow an existing environment
  name (`ShadowedBinder`).
- `Pretty.hs` — renders `Term` back to readable named syntax, plus a raw
  de Bruijn-index view (`\.\.1 (1 0)`) for `:debruijn on`.
- `Main.hs` — the REPL loop and environment (`Map String Term` in an `IORef`).

## REPL commands

```
let name = expr     bind a name (overrides any prior binding of the same name)
<expr>               evaluate and print
:env                 list current bindings
:step on|off          show every beta-reduction step
:debruijn on|off      also show raw de Bruijn index form
:help
:quit
```

## Variable scoping rules

- **No free variables.** Every variable must be bound by an enclosing lambda
  or a prior `let`; otherwise compilation fails with `unbound variable: x`.
- **`let` may rebind freely.** `let id = ...` followed by another
  `let id = ...` simply replaces the old binding
- **Lambda parameters may not shadow the environment.** `\true -> true` fails
  with `lambda binder shadows existing binding: true` if `true` is already
  `let`-bound, since that's almost always a mistake (use a different
  parameter name, or `let true = ...` if you actually mean to redefine it).
  Lambda binders may still shadow *each other* as is convecntional in ordinary
  lambda calculus
  e.g. `(\x -> \x -> x) (\y -> y)` is fine.

## Reduction limits

Reduction is capped two ways:

- **Step limit** (default 10000): the max number of beta-reduction steps
  before giving up, guarding against non-terminating terms (e.g. the Omega
  combinator `(\x -> x x) (\x -> x x)`).
- **Size cap** (default 100000 AST nodes): if a single reduction step would
  produce a term larger than this, reduction stops immediately. For bare
  expressions, the last term still within the cap is printed. For `let`
  bindings, the original unreduced expression is saved. 
  
The size cap only applies to terms *produced by a reduction step* — it never
rejects what you type in.

If a `let`-bound expression hits either limit, the REPL does **not** store
the partial result. It saves the *original, unreduced* expression instead, so
later references to that name re-attempt reduction from scratch on demand:

```
λ> let omega = \x -> x x
omega = \x -> x x
λ> let bigOmega = omega omega
bigOmega did not normalize within 10000 steps; saved the original
(unreduced) expression instead of a partial result. ((\x -> x x) (\x -> x x))
```

## Example session

```
λ> let zero = \f x -> x
zero = \x -> \y -> y
λ> let succ = \n f x -> f (n f x)
succ = \x -> \y -> \z -> y (x y z)
λ> let two = succ (succ zero)
two = \x -> \y -> x (x y)
λ> let plus = \m n f x -> m f (n f x)
plus = \x -> \y -> \z -> \w -> x z (y z w)
λ> plus two two
\x -> \y -> x (x (x (x y)))
λ> :debruijn on
λ> two
\x -> \y -> x (x y)    [\.\.1 (1 0)]
λ> :step on
λ> (\x -> x x) (\y -> y)
  0: (\x -> x x) (\x -> x)
  1: (\x -> x) (\x -> x)
  2: \x -> x
```
