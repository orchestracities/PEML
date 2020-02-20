{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
-- | Pretty-printing of PEML expressions.
module Peml.Dump.PrettyPrint
  ( drawPeml
  , printPeml
  , vast
  )
where

import Prelude.Unicode
import Data.Text (unpack)
import Data.Tree (Tree (..), drawTree)
import GHC.Generics ((:+:) (..))

import Peml.Lang.Ast
import Peml.Lang.Builder


-- | Algebra for the PEML interpreter that maps expression trees to
-- string trees. Running the interpreter:
--
-- @
--     expr :: PemlExpr
--     foldPeml render expr :: Tree String
-- @
class Functor f ⇒ Treealize f where
  render ∷ f (Tree String) → Tree String

instance Treealize Empty where
  render _ = Node "∅" []

printLit ∷ Literal → String
printLit Nil = "null"
printLit (Z v) = show v
printLit (R v) = show v
printLit (Bln v) = show v
printLit (Txt v) = unpack v

instance Treealize Lit where
  render (Lit v) = Node (printLit v) []

instance Treealize Seq where
  render (Seq ts) = Node "Seq" ts

instance Treealize Map where
  render (Map kts) = Node "Map" $ fmap toNode kts
    where
      toNode (key, Node v []) = Node (unpack key ++ ": " ++ v) []
      toNode (key, t) = Node (unpack key ++ ":") [t]

instance (Treealize f, Treealize g) ⇒ Treealize (f :+: g) where
  render (L1 f) = render f
  render (R1 g) = render g

-- | Draw a PEML expression as a tree---ASCII art!
drawPeml ∷ PemlExpr → String
drawPeml = drawTree ∘ foldPeml render

-- | Pretty-print PEML expression to 'stdout'.
printPeml ∷ PemlExpr → IO ()
printPeml = putStr ∘ drawPeml

-- | View AST---vastly misnamed :-)
-- Build the PEML expression and pretty-print it to 'stdout' as a tree.
vast :: ExprBuilder → IO ()
vast = printPeml ∘ build
