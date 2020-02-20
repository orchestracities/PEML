{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
-- | Conversion of PEML to YAML.
module Peml.Dump.Yaml
  ( YamlAst
  , asYaml
  , dumpYaml
  , printYaml
  , writeYaml
  , yamlify
  )
where

import Prelude.Unicode
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B (putStr, writeFile)
import Data.YAML (Node (..), ToYAML (..), (.=), mapping, encode)
import GHC.Generics ((:+:) (..))

import Peml.Lang.Ast
import Peml.Lang.Builder


type YamlAst = Node ()

-- | Algebra for the PEML interpreter that converts a PEML AST to a
-- YAML one. Running the interpreter:
--
-- @
--     expr :: PemlExpr
--     foldPeml toYamlAst expr :: YamlAst
-- @
class Functor f ⇒ ToYamlAst f where
  toYamlAst ∷ f YamlAst → YamlAst

instance ToYamlAst Empty where
  toYamlAst _ = mapping []

instance ToYamlAst Lit where
  toYamlAst (Lit v) = convert v
    where
      convert Nil = mapping []
      convert (Z z) = toYAML z
      convert (R r) = toYAML r
      convert (Bln b) = toYAML b
      convert (Txt t) = toYAML t

instance ToYamlAst Seq where
  toYamlAst (Seq ns) = toYAML ns

instance ToYamlAst Map where
  toYamlAst (Map kns) = mapping [ k .= n | (k, n) ← kns ]

instance (ToYamlAst f, ToYamlAst g) ⇒ ToYamlAst (f :+: g) where
  toYamlAst (L1 f) = toYamlAst f
  toYamlAst (R1 g) = toYamlAst g

-- | Convert the input PEML expression tree to the equivalent YAML
-- AST.
asYaml ∷ PemlExpr → YamlAst
asYaml = foldPeml toYamlAst

-- | Serialise PEML expressions to YAML.
-- Each expression produces exactly one YAML document.
dumpYaml ∷ [PemlExpr] → ByteString
dumpYaml = encode ∘ fmap asYaml

-- | Print the output of 'dumpYaml' to 'stdout'.
printYaml ∷ [PemlExpr] → IO ()
printYaml = B.putStr ∘ dumpYaml

-- | Write the output of 'dumpYaml' to the specified file.
writeYaml ∷ FilePath → [PemlExpr] → IO ()
writeYaml path = B.writeFile path ∘ dumpYaml

-- | Build the PEML expression and dump it as YAML to 'stdout'.
yamlify ∷ ExprBuilder → IO ()
yamlify = printYaml ∘ pure ∘ build
