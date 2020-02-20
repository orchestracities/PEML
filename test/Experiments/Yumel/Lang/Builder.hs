{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, InstanceSigs #-}
{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Experiments.Yumel.Lang.Builder where

import Prelude.Unicode
import Control.Arrow ((***))
import Data.Monoid (Monoid)
import Data.Text (Text)

import Experiments.Yumel.Lang.Ast


newtype Builder τ ξ = B { unB ∷ (τ, ξ) }
  deriving Show

getAst ∷ Builder τ ξ → τ
getAst = fst ∘ unB

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


class Taggable ξ τ where
  type Value ξ τ ∷ *
  type Result ξ τ ∷ *

  (~:) ∷ Text → Value ξ τ → Builder (Result ξ τ) ()
  -- TODO needs AllowAmbiguousTypes to compile

instance Taggable YumelTree YumelTree where
  type Value YumelTree YumelTree = Builder YumelTree ()
  type Result YumelTree YumelTree = YumelTree

  (~:) ∷ Text → Builder YumelTree () → Builder YumelTree ()
  (~:) name = B ∘ (Tag name *** id) ∘ unB


instance Taggable YumelTree KeyVal where
  type Value YumelTree KeyVal = Builder YumelTree ()
  type Result YumelTree KeyVal = [KeyVal]

  (~:) ∷ Text → Builder YumelTree () → Builder [KeyVal] ()
  (~:) name = B ∘ (f *** id) ∘ unB
    where f = pure ∘ (KeyVal name)

instance Taggable KeyVal YumelTree where
  type Value KeyVal YumelTree = Builder [KeyVal] ()
  type Result KeyVal YumelTree = YumelTree

  (~:) ∷ Text → Builder [KeyVal] () → Builder YumelTree ()
  (~:) name = B ∘ (f *** id) ∘ unB
    where f = Tag name ∘ Map

(~>) ∷ () → Builder [KeyVal] () → Builder YumelTree ()
(~>) () = B ∘ (Map *** id) ∘ unB

-- TODO In principle (~:) and (~>) should be a replacement for all
-- combinators below, but see WTF at the very bottom.

(=:) ∷ Text → Builder YumelTree ξ → Builder YumelTree ξ
(=:) name = B ∘ (Tag name *** id) ∘ unB

(>:) ∷ Text → Builder YumelTree ξ → Builder [KeyVal] ξ
name >: B (t, x) = B ([KeyVal name t], x)

(>=:) ∷ Text → Builder [KeyVal] ξ → Builder YumelTree ξ
name >=: B (ps, x) = B (Tag name $ Map ps, x)

(><) ∷ Builder [KeyVal] ξ → Builder YumelTree ξ
(><) (B (ps, x)) = B (Map ps, x)

int ∷ Integer → Builder YumelTree ()
int x = B (Lit ∘ Z $ x, ())

txt ∷ Text → Builder YumelTree ()
txt x = B (Lit ∘ Txt $ x, ())

bln ∷ Bool → Builder YumelTree ()
bln x = B (Lit ∘ Bln $ x, ())

u1 = int 1
u2 = int 2

-- u3 :: Builder YumelTree ()
u3 = do
  u1
  u2

u4 = (><) $ do
  "u1" >: u1
  "u2" >: u2

u5 = do
  "u4" =: u4

u6 = do
  "u3" =: u3

u7 = do
  u5
  u6

u8 = (><) $ do
  "kind" >: txt "api"
  "spec" >: do
    "port" =: int 8080
    "host" >=: do
      "name" >: txt "no.where"
      "real" >: bln False
    "items" =: do
      (><) $ do
        "key" >: txt "k1"
        "val" >: int 1
      (><) $ do
        "key" >: txt "k2"
        "val" >: int 2

k :: Text
k = "k"

-- WTF. This won't type-check
-- (~:) k (int 1)

{-
u9 = () ~> do
  "kind" ~: txt "api"
  "spec" ~: do
    "port" ~: int 8080
    "host" ~: do
      "name" ~: txt "no.where"
      "real" ~: bln False
    "items" ~: do
      () ~> do
        "key" ~: txt "k1"
        "val" ~: int 1
      () ~> do
        "key" ~: txt "k2"
        "val" ~: int 2
-}

view = putStrLn . drawYumel . getAst
