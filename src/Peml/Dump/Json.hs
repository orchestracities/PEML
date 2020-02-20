{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
-- | Conversion of PEML to JSON.
module Peml.Dump.Json
  ( asJson
  , dumpJson
  , printJson
  , writeJson
  , jsonify
  )
where

import Prelude.Unicode
import Data.Aeson (ToJSON (..), object, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Value (..), emptyObject)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B (putStr, writeFile)
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)
import GHC.Generics ((:+:) (..))

import Peml.Lang.Ast
import Peml.Lang.Builder


-- | Algebra for the PEML interpreter that converts a PEML AST to a
-- JSON one. Running the interpreter:
--
-- @
--     expr :: PemlExpr
--     foldPeml toJsonAst expr :: Value
-- @
class Functor f ⇒ ToJsonAst f where
  toJsonAst ∷ f Value → Value

instance ToJsonAst Empty where
  toJsonAst _ = emptyObject

instance ToJsonAst Lit where
  toJsonAst (Lit v) = convert v
    where
      convert Nil = Null
      convert (Z z) = toJSON z
      convert (R r) = toJSON r
      convert (Bln b) = toJSON b
      convert (Txt t) = toJSON t

instance ToJsonAst Seq where
  toJsonAst (Seq vs) = toJSON vs

instance ToJsonAst Map where
  toJsonAst (Map kvs) = object kvs

instance (ToJsonAst f, ToJsonAst g) ⇒ ToJsonAst (f :+: g) where
  toJsonAst (L1 f) = toJsonAst f
  toJsonAst (R1 g) = toJsonAst g

-- | Convert the input PEML expression tree to the equivalent JSON
-- AST.
asJson ∷ PemlExpr → Value
asJson = foldPeml toJsonAst

-- | Serialise a PEML expression to JSON.
dumpJson ∷ PemlExpr → ByteString
dumpJson = encode ∘ asJson

-- | Print the output of 'dumpJson' to 'stdout'.
printJson ∷ PemlExpr → IO ()
printJson = B.putStr ∘ dumpJson

-- | Write the output of 'dumpJson' to the specified file.
writeJson ∷ FilePath → PemlExpr → IO ()
writeJson path = B.writeFile path ∘ dumpJson

-- | Build the PEML expression and pretty-print it as JSON to 'stdout'.
jsonify ∷ ExprBuilder → IO ()
jsonify = B.putStrLn ∘ encodePretty ∘ asJson ∘ build
