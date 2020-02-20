{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE GADTs, RankNTypes #-}
module Experiments.Yumel.Lang.Ast where

import Prelude.Unicode
import Data.Text (Text, unpack)
import Data.Tree (Tree (..), drawTree)


data Literal = Nil | Z Integer | R Double | Bln Bool | Txt Text
  deriving Show

data YumelTree = Empty
               | Lit Literal
               | Tag Text YumelTree
               | Seq [YumelTree]
               | Map [KeyVal]
  deriving Show

data KeyVal = KeyVal Text YumelTree
  deriving Show

-- tweak AST to make it work nicely w/ Builder monad.

instance Semigroup YumelTree where
  Empty <> t        = t
  t <> Empty        = t
  Seq ts <> Seq ts' = Seq $ ts <> ts'
  Seq ts <> t       = Seq $ ts <> [t]
  t <> Seq ts       = Seq $ t : ts
  t <> t'           = Seq [t, t']
-- NOTE. Dirty hack.
-- The above really doesn't make alot of sense in general, but it works
-- in the context of the Builder monad, since there we're basically listing
-- nodes, e.g.
--
-- do
--   n1
--   n2                        \    kinda equivalent to:
--   "another seq": do      ----\
--      k1                  ----/    [n1, n2, [k1, k2]]
--      k2                     /
--   n3
--

instance Monoid YumelTree where
  mempty = Empty

{- GADT alternative design

data Leaf
data Tagged

data YumelTree τ where
  Empty ∷ YumelTree Leaf
  Lit ∷ Literal → YumelTree Leaf
  Tag ∷ Text → YumelTree τ → YumelTree Tagged
  Seq ∷ (∀ τ . [YumelTree τ]) → YumelTree τ
  Map ∷ [YumelTree Tagged] → YumelTree Tagged

-- x :: YumelTree Tagged
x = Seq
  [ Lit (Z 3)
  , Tag "k1" $ Lit (Z 1)
  , Tag "k2" $ Lit (Z 2)
  , Lit (Z 3)
  ]

y = Map [
  Lit (Z 3)
        ]
-}

drawYumel ∷ YumelTree → String
drawYumel = drawTree ∘ printableAst

printLit ∷ Literal → String
printLit Nil = "null"
printLit (Z v) = show v
printLit (R v) = show v
printLit (Bln v) = show v
printLit (Txt v) = unpack v

printableNamed ∷ Text → YumelTree → Tree String
printableNamed name (Lit v) = Node (unpack name ++ ": " ++printLit v) []
printableNamed name t = Node (unpack name ++ ":") [printableAst t]

printableAst ∷ YumelTree → Tree String
printableAst Empty = Node "∅" []
printableAst (Lit v) = Node (printLit v) []
printableAst (Tag name t) = printableNamed name t
printableAst (Seq ts) = Node "Seq" $ fmap printableAst ts
printableAst (Map ps) = Node "Map" $ fmap printKv ps
  where
    printKv (KeyVal name t) = printableNamed name t

t0 = Empty
t1 = Lit (Z 1)
t2 = Lit (Z 2)
t3 = Seq [t0, t1, t2]
t4 = Map [KeyVal "t0" t0, KeyVal "t1" t1]
t5 = Tag "t4" t4
t6 = Tag "t3" t3
t7 = Seq [t5, t6]
t8 = Map
  [ KeyVal "kind" $ Lit $ Txt "api"
  , KeyVal "spec" $ Seq
    [ Tag "port" $ Lit $ Z 8080
    , Tag "host" $ Map
      [ KeyVal "name" $ Lit $ Txt "no.where"
      , KeyVal "real" $ Lit $ Bln False
      ]
    ]
  ]
