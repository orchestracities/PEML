{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- | EDSL to build complex PEML expressions without visual clutter,
-- using a lean, YAML-like syntax. You assemble PEML expressions in
-- the 'ExprBuilder' monad using the sequencing '(-:)' and tag '=:'
-- operators, then you use 'build' to get the expression out of the
-- monad---see 'Examples' package. This EDSL lets you assemble plain
-- sequences or maps as well as composite structures involving the
-- two at any nesting depth. But you can't build all possible PEML
-- expressions. Specifically, you can't build
--
-- 1. Empty expression. Use 'pemlEmpty' from 'Peml.Lang.Ast' for that.
-- 2. Literals. If you need an expression made up by a single literal
--    use 'pemlNil', 'pemlInt', etc. exported by 'Peml.Lang.Ast'.
-- 3. Lists of tags. The EDSL turns any list of tags in the expression
--    tree into a map. If you really need a list of tags, then use the
--    smart constructors of 'Peml.Lang.Ast' to build it.
--
module Peml.Lang.Builder
  ( ToPeml (..)
  , ExprBuilder
  , build
  , (-:)
  , (=:)
  , skip
  )
where

import Prelude.Unicode
import Control.Arrow ((***))
import Data.Text (Text, pack)

import Peml.Lang.Ast
import Peml.Lang.Tag


-- | Lifting of values into expressions.
class ToPeml ξ where
  -- | Make an expression out of the input value.
  toPeml ∷ ξ → PemlExpr

instance ToPeml Integer where
  toPeml = pemlInt

instance ToPeml Double where
  toPeml = pemlDbl

instance ToPeml Bool where
  toPeml = pemlBln

instance ToPeml Text where
  toPeml = pemlTxt

instance ToPeml String where
  toPeml = pemlTxt ∘ pack

instance ToPeml Literal where
  toPeml = pemlLit

instance ToPeml PemlExpr where
  toPeml = id


-- monadic sequencing of expressions.
-- TODO use Control.Monad.Writer instead of home-brewed Builder?

-- Syntactic sugar type to take advantage of `do` notation to build
-- PEML expressions. The basic idea is that if we make it a simplified
-- Writer monad, then we can do stuff like:
--
--     x :: Builder [Int] ()
--     x = do
--       B ([1], ())
--       B ([2], ())
--
--     show x ~~> B ([1, 2], ())
--
-- which comes in handy to get rid of syntactic noise when building
-- a PEML expression, see below.
-- The τ param accumulates expression elements built so far whereas ξ
-- is just a dummy we need for the monad stuff.
newtype Builder τ ξ = B { unB ∷ (τ, ξ) }
  deriving Show

-- ∀ τ . Builder τ ≅ identity
instance Functor (Builder τ) where
  fmap f = B ∘ (id *** f) ∘ unB

-- combine monoid values.
instance Monoid τ ⇒ Applicative (Builder τ) where
  pure = B ∘ (mempty, )
  B (t, f) <*> B (t', x) = B (t <> t', f x)

-- combine monoid values.
instance Monoid τ ⇒ Monad (Builder τ) where
  B (t, x) >>= f = B (t <> t', y)
    where B (t', y) = f x


-- expression builder.

-- | Expression building context.
-- It's a monad that lets you build a PEML expression by sequencing
-- the '(-:)' and '(=:)' operators. Use the 'build' function to get
-- the expression you've built.
type ExprBuilder = Builder (Seq PemlExpr) ()

instance ToPeml ExprBuilder where
  toPeml (B (Seq xs, _)) = pemlSeq xs

injectExpr ∷ PemlExpr → ExprBuilder
injectExpr = B ∘ (, ()) ∘ Seq ∘ pure

-- | Assemble an expression from a building context.
-- Notice that sequences of tags get collapsed to a map which is
-- probably what you want in the majority of cases, but since we
-- do this under the bonnet, you'll never be able to use the
-- 'ExprBuilder' to assemble a sequence of tags. If you really
-- can't live without that gawky thingie, you can always build it
-- yourself using PEML AST smart constructors.
build ∷ ExprBuilder → PemlExpr
build = mapify ∘ toPeml


-- combinators

-- | Do-nothing combinator. Handy for adding expressions conditionally
-- to the building context.
skip ∷ ExprBuilder
skip = B (mempty, ())

-- | Append an item to the building context.
(-:) ∷ ToPeml ξ ⇒ ξ → ExprBuilder
(-:) = injectExpr ∘ toPeml

-- | Append a named item (tag) to the building context.
(=:) ∷ ToPeml ξ ⇒ String → ξ → ExprBuilder
(=:) name = injectExpr ∘ pemlTag (pack name) ∘ toPeml
-- NOTE. String type.
-- Ideally it'd be Text, but a String makes it for a cleaner syntax
-- since (w/o OverloadedStrings turned on) we can just write e.g.
--
--     ex = do
--       "k" =: "x"
--       (-:) "y"
--
-- instead of (OverloadedStrings on and name param = Text)
--
--     ex = do
--       "k" =: ("x" :: Text)
--       (-:) ("y" :: Text)
--
-- Now if performance is your thing, you're welcome to turn on
-- OverloadedStrings, type-annotate values and curse me for not
-- having made name :: Text but since name will probably be just
-- a handful of chars in most cases, I don't think it'll hurt
-- performance in practice. Famous last words.


{- NOTE. Can we use the EDSL to build **any** PEML expression?
Yes, in principle---see code below w/ dedicated builders for
maps and sequences. But then you need more combinators too since
you'll have to be able to know if (=:) happens within a sequence
or a map context. More combinators make, IMO, the EDSL less readable.
Perhaps there's a way to overload (=:) so that it does different
things in sequence and map contexts. I tried doing that with
type families but failed miserably---if any type whizz out there
happens to read this, you're help would be greatly appreciated!
For the record, I tried something along the lines of

class Taggable ξ τ where
  type Value ξ τ ∷ *
  type Result ξ τ ∷ *

  (=:) ∷ Text → Value ξ τ → Builder (Result ξ τ) ()
  -- needs AllowAmbiguousTypes to compile

but then got stuck since I couldn't manage to build expressions...
(Have a look at 'Experiments.Yumel.Lang.Builder' for a similar failed
attempt with a plain algebraic data type as AST.)
Anyhoo the code below lets you build any PEML expression but like
I said you force the user to juggle more combinators.


newtype SeqBuilder ξ = SeqBuilder (Seq PemlExpr, ξ)
  deriving (Functor, Applicative, Monad) via (Builder (Seq PemlExpr))

newtype MapBuilder ξ = MapBuilder (Map PemlExpr, ξ)
  deriving (Functor, Applicative, Monad) via (Builder (Map PemlExpr))


class Monad m ⇒ BuilderMonad m where
  addMap ∷ Map PemlExpr → m ()

instance BuilderMonad SeqBuilder where
  addMap (Map kvs) = SeqBuilder (Seq ∘ pure ∘ pemlMap $ kvs, ())

instance BuilderMonad MapBuilder where
  addMap = MapBuilder ∘ (, ())


instance ToPeml (SeqBuilder ξ) where
  toPeml (SeqBuilder (Seq xs, _)) = pemlSeq xs

instance ToPeml (MapBuilder ξ) where
  toPeml (MapBuilder (Map kvs, _)) = pemlMap kvs


(=:) ∷ (BuilderMonad m, ToPeml ξ) ⇒ Text → ξ → m ()
(=:) name = addMap ∘ tag name ∘ toPeml
  where
    tag n = Map ∘ pure ∘ (n, )

(~>) ∷ () → MapBuilder () → MapBuilder ()
(~>) () = id


x1 :: SeqBuilder ()
x1 = do
  "a" =: ("x" :: Text)
  "b" =: True
  "c" =: (3 :: Integer)

-- x2 :: MapBuilder ()
x2 = () ~> do
  "a" =: ("x" :: Text)
  "b" =: True
  "c" =: (3 :: Integer)

x3 :: SeqBuilder ()
x3 = do
  "a" =: ("x" :: Text)
  "b" =: True
  "c" =: (
    () ~> do
    "x" =: (5 :: Integer)
    "y" =: False
    )

-}
