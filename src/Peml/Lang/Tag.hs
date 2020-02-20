{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections, TypeOperators #-}
-- | A PEML tag is a 'Map' with just one entry.
-- It's just a convenience concept that comes in handy for building
-- 'PemlExpr's. So we have some utils down below just for that.
module Peml.Lang.Tag
  ( pemlTag
  , isTag
  , mapify
  )
where

import Prelude.Unicode
import Data.Text (Text)
import GHC.Generics ((:+:) (..))

import Peml.Lang.Ast


-- | Syntactic sugar for a one-entry 'Map'.
pemlTag ∷ Text → PemlExpr → PemlExpr
pemlTag name = pemlMap ∘ pure ∘ (name, )

-- | Algebra for the PEML interpreter that figures out if a given
-- expression is a tag, i.e. a single-entry 'Map'.
-- Running the interpreter:
--
-- @
--     expr :: PemlExpr
--     foldPeml _isTag expr :: Bool
-- @
class Functor f ⇒ IsTag f where
  _isTag ∷ f Bool → Bool

instance IsTag Empty where
  _isTag = const False

instance IsTag Lit where
  _isTag = const False

instance IsTag Seq where
  _isTag = const False

instance IsTag Map where
  _isTag (Map kvs) = length kvs == 1

instance (IsTag f, IsTag g) ⇒ IsTag (f :+: g) where
  _isTag (L1 f) = _isTag f
  _isTag (R1 g) = _isTag g

-- | Is the given expression a tag?
isTag ∷ PemlExpr → Bool
isTag = foldPeml _isTag


-- | Algebra for the PEML interpreter that turns a 'Seq' of tags into
-- a map and leaves any other expression be.
-- Running the interpreter:
--
-- @
--     expr :: PemlExpr
--     foldPeml _mapify expr :: PemlExpr
-- @
class Functor f ⇒ Mapify f where
  _mapify ∷ f PemlExpr → PemlExpr

instance Mapify Empty where
  _mapify _ = pemlEmpty

instance Mapify Lit where
  _mapify (Lit v) = pemlLit v

instance Mapify Seq where
  _mapify (Seq xs) | all isTag xs = pemlMap merged
                   | otherwise    = pemlSeq xs
    where
      Map merged = mconcat ∘ fmap getMap $ xs

      -- TODO don't pattern match on PemlExpr!!
      -- find a better way of doing this.
      getMap (In (R1 (R1 (R1 (Map kvs))))) = Map kvs
      getMap _ = Map []

instance Mapify Map where
  _mapify (Map kvs) = pemlMap kvs

instance (Mapify f, Mapify g) ⇒ Mapify (f :+: g) where
  _mapify (L1 f) = _mapify f
  _mapify (R1 g) = _mapify g

-- | Run the 'Mapify' interpreter on the input expression.
mapify ∷ PemlExpr → PemlExpr
mapify = foldPeml _mapify
