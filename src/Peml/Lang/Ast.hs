{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs, KindSignatures, TypeOperators #-}
-- | PEML abstract syntax machinery.
-- Read code comments below if you'd like to understand how to extend
-- PEML or write expression interpreters. Assemble expressions with
-- the provided 'peml*' smart constructors or use the expression building
-- EDSL exported by the 'Builder' module.
module Peml.Lang.Ast where

import Prelude.Unicode

import Data.Text (Text)
import GHC.Generics ((:+:) (..))

{-
Basic idea: grammar as initial algebra.

Wot!? Yea, I know, but there's a reason to my madness. In fact, this
is the best solution to the expression problem I know of and, according
to the interwebs, is also quite efficient since GHC has an easier time
optimising non-recursive data.

Here's the target grammar

    Literal = Nil | Z Integer | R Double | Bln Bool | Txt Text

    Peml = Empty
         | Lit Literal
         | Seq [Expr]
         | Map [(Text, Expr)]

Below is the machinery to get it as a fixed point of the endo-functor

    F = E + L + S + M

where E, L, S, M functorify Empty, Lit, etc.

See e.g.
- "Data types à la carte", Wouter Swierstra
- § 10 of "Category Theory", Steve Awodey
-}


-- represent grammar terms with functors to get around the expression
-- problem.

data Literal = Nil | Z Integer | R Double | Bln Bool | Txt Text
  deriving Show

data Lit ξ = Lit Literal
  deriving Show

instance Functor Lit where
  fmap _ (Lit v) = (Lit v)

newtype Seq ξ = Seq [ξ]
  deriving Show
  deriving Functor via []
  deriving (Semigroup, Monoid) via [ξ]

newtype Map ξ = Map [(Text, ξ)]
  deriving Show
  deriving (Semigroup, Monoid) via [(Text, ξ)]

instance Functor Map where
  fmap f (Map kvs) = Map $ fmap (\(k, v) → (k, f v)) kvs

data Empty ξ = Empty
  deriving Show

instance Functor Empty where
  fmap _ _ = Empty


-- deep embedding à la Gibbons, see
-- "Folding Domain-Specific Languages: Deep and Shallow Embeddings"

data Deep ∷ (* → *) → * where
  In ∷ Functor f ⇒ f (Deep f) → Deep f

type PemlF = Empty :+: Lit :+: Seq :+: Map
type PemlExpr = Deep PemlF

foldPeml ∷ (PemlF ξ → ξ) → PemlExpr → ξ
foldPeml alg (In initAlg) = alg ∘ fmap (foldPeml alg) $ initAlg

-- TODO rather use `Fix` and `cata` from `Data.Functor.Fixedpoint`
-- in unification-fd?
-- See: https://hackage.haskell.org/package/unification-fd

{-
Yup, it looks evil and likely came out of an occult ritual where
participants were high on a yet-to-be-known-to-man substance. But
there's a "simple" maths explanation---no I'm not high too. Well,
simple if you eat algebra for breakfast. In fact, there's a unique
morphism φ = (foldPeml alg) from the PemlF initial algebra to any
other PemlF-algebra.
See e.g.
- https://ncatlab.org/nlab/show/initial+algebra+of+an+endofunctor

In our case all that boils down to

           φ ∘ initAlg = alg ∘ fmap φ

where φ = (foldPeml alg), since

                      fmap φ
    PemlF PemlExpr  ----------->  PemlF ξ

         |                          |
 initAlg | ≅                    alg |
         |                          |
         v                          v
                         φ
      PemlExpr      ----------->    ξ
         \
          = colim (0 → PemlF 0 → PemlF² 0  → ..)
-}

-- convenience smart constructors.
-- NOTE. Extensibility.
-- So we've "functorialised" our grammar to be able to add **both** new
-- terms and interpreters **without** changing existing ones while keeping
-- strong static type safety---i.e. expression problem solved, yay! But
-- there's one small snag. Building a PemlExpr can be a royal pain in the
-- backside b/c of the annoying co-products in between. So we've implemented
-- smart constructors down below but as you can see, if you add a new
-- term (= functor) then you'll also have to rearrange the R1/L1 permutations
-- which can also be annoying. If that really gets in your way, there's
-- a solution to this minor inconvenience too as explained in § 4 of
-- "Data types à la carte".

pemlSeq ∷ [PemlExpr] → PemlExpr
pemlSeq = In ∘ R1 ∘ R1 ∘ L1 ∘ Seq

pemlMap ∷ [(Text, PemlExpr)] → PemlExpr
pemlMap = In ∘ R1 ∘ R1 ∘ R1 ∘ Map

pemlEmpty ∷ PemlExpr
pemlEmpty = In ∘ L1 $ Empty

pemlLit ∷ Literal → PemlExpr
pemlLit = In ∘ R1 ∘ L1 ∘ Lit

pemlNil ∷ PemlExpr
pemlNil = pemlLit Nil

pemlInt ∷ Integer → PemlExpr
pemlInt = pemlLit ∘ Z

pemlDbl ∷ Double → PemlExpr
pemlDbl = pemlLit ∘ R

pemlBln ∷ Bool → PemlExpr
pemlBln = pemlLit ∘ Bln

pemlTxt ∷ Text → PemlExpr
pemlTxt = pemlLit ∘ Txt
