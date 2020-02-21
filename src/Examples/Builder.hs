{-# LANGUAGE UnicodeSyntax #-}
module Examples.Builder where

import Peml

-- use 'vast' to build and pretty-print expression to 'stdout'.
print_example ∷ IO ()
print_example = vast x1


x1 ∷ ExprBuilder
x1 = do
  "a" =: "x"             -- (1)
  "b" =: True
  "c" =: (3 :: Integer)  -- (2)
-- NOTE
-- (1) For better performance turn on OverloadedStrings and add type
-- annotation as in:
--    "a" =: ("x" :: Text)
-- (2) You can use Z and R ctors to avoid verbose type annotations for
-- numbers, see below.

x2 ∷ ExprBuilder
x2 = do
  "a" =: "x"
  (-:) "y"
  (-:) (Z 21)     -- Z is the maths symbol for integers.
  "b" =: True
  "c" =: Z 3
  "d" =: do
    (-:) (R 1.5)  -- R is the maths symbol for real numbers.
    (-:) $ R 2.7  -- you can avoid parens with the $ operator.

x3 ∷ ExprBuilder
x3 = do
  "a" =: "x"
  "b" =: True
  "c" =: do
    "x" =: Z 5
    "y" =: False

x4 ∷ ExprBuilder
x4 = do
  (-:) "xx"
  (-:) Nil  -- no value PEML literal.

x5 ∷ ExprBuilder
x5 = do
  (-:) "a"
  (if "?" == "" then ((-:) "b") else skip)
  "c" =: do
    "x" =: Z 5
    (if "?" == "" then ("z" =: "!") else skip)
    "y" =: False
    (if "?" == "?" then ("w" =: "wada") else skip)
    maybe skip ("m" =:) $ Just $ do
      let hv = Nothing :: Maybe String
      maybe skip ("h" =:) hv
      maybe skip ("k" =:) $ Just $ Z 3
      "l" =: do
        let v = Nothing :: Maybe String
        maybe skip (-:) v
        maybe skip (-:) $ Just "I'm in!"
