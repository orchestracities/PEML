{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BlockArguments #-}
module Examples.Yaml where

import Peml


-- use 'yamlify' to view YAML on 'stdout'.
view_example ∷ IO ()
view_example = yamlify ex_2_3


-- Examples from § 2.1 of Yaml 1.2 spec
-- https://yaml.org/spec/1.2/spec.html

-- Example 2.1. Sequence of Scalars
-- (ball players)
ex_2_1 ∷ ExprBuilder
ex_2_1 = do
  (-:) "Mark McGwire"
  (-:) "Sammy Sosa"
  (-:) "Ken Griffey"

-- Example 2.2. Mapping Scalars to Scalars
-- (player statistics)
ex_2_2 ∷ ExprBuilder
ex_2_2 = do
  "hr"  =: Z 65    -- Home runs
  "avg" =: R 0.278 -- Batting average
  "rbi" =: Z 147   -- Runs Batted In

-- Example 2.3. Mapping Scalars to Sequences
-- (ball clubs in each league)
ex_2_3 ∷ ExprBuilder
ex_2_3 = do
  "american" =: do
    (-:) "Boston Red Sox"
    (-:) "Detroit Tigers"
    (-:) "New York Yankees"
  "national" =: do
    (-:) "New York Mets"
    (-:) "Chicago Cubs"
    (-:) "Atlanta Braves"

-- Example 2.4. Sequence of Mappings
-- (players’ statistics)
ex_2_4 ∷ ExprBuilder
ex_2_4 = do
  (-:) do  -- (*) see below note
    "name" =: "Mark McGwire"
    "hr"   =: Z 65
    "avg"  =: R 0.278
  (-:) do
    "name" =: "Sammy Sosa"
    "hr"   =: Z 63
    "avg"  =: R 0.288

-- (*) NOTE. BlockArguments.
-- In Haskell 2010, do blocks can't be passed in straight as function
-- args. BlockArguments is a syntactic sugar extension that lifts that
-- restriction. Without BlockArguments, you'd write the above as:
--
--     (-:) $ do
--         ...
--
-- or use parens around the do block.

-- Example 2.5. Sequence of Sequences
ex_2_5 ∷ ExprBuilder
ex_2_5 = do
  (-:) do
    (-:) "name"
    (-:) "hr"
    (-:) "avg"
  (-:) do
    (-:) "Mark McGwire"
    (-:) (Z 65)
    (-:) (R 0.278)
  (-:) do
    (-:) "Sammy Sosa"
    (-:) $ Z 63
    (-:) $ R 0.288

-- Example 2.6. Mapping of Mappings
ex_2_6 ∷ ExprBuilder
ex_2_6 = do
  "Mark McGwire" =: do
    "hr"  =: Z 65
    "avg" =: R 0.278
  "Sammy Sosa" =: do
    "hr"  =: Z 63
    "avg" =: R 0.288
