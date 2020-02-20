{-# LANGUAGE UnicodeSyntax #-}
module Examples.Json where

import Examples.Yaml
import Peml

-- use 'jsonify' to view JSON on 'stdout'.
view_example' ∷ IO ()
view_example' = jsonify ex_2_3

-- use 'ex_*' builders imported from 'Examples.Yaml'
