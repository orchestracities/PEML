-- | Convenience module providing access to basic functionality:
--
-- * PEML expression building.
-- * Pretty-printing of PEML expression trees.
-- * Serialisation of PEML expressions to YAML and JSON.
--
-- Import other modules separately if you need more advanced stuff,
-- e.g. fiddling with ASTs.
module Peml
  ( Literal (..)
  , module Peml.Dump.Json
  , module Peml.Dump.PrettyPrint
  , module Peml.Dump.Yaml
  , module Peml.Lang.Builder
  )
where

import Peml.Dump.Json
import Peml.Dump.PrettyPrint
import Peml.Dump.Yaml
import Peml.Lang.Ast
import Peml.Lang.Builder
