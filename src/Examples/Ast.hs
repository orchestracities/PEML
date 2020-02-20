{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Examples.Ast where

import Peml.Dump.PrettyPrint
import Peml.Lang.Ast
import Peml.Lang.Tag


-- use 'printPeml' to pretty-print expression to 'stdout'.
print_example ∷ IO ()
print_example = printPeml mixed_sequence


-- no content expressions

p_empty, empty_seq, empty_map ∷ PemlExpr
p_empty = pemlEmpty
empty_seq = pemlSeq []
empty_map = pemlMap []


-- literal expressions

p_nil, p_1, p_2, p_wadawada, p_true ∷ PemlExpr
p_nil = pemlNil
p_1 = pemlInt 1
p_2 = pemlInt 2
p_wadawada = pemlTxt "wada wada"
p_true = pemlBln True


-- simple composite expressions

plain_seq, plain_map, p_1_tag, p_2_tag ∷ PemlExpr
plain_seq = pemlSeq [p_1, p_nil, p_true]
plain_map = pemlMap [ ("junk", p_true)
                    , ("description", p_wadawada)
                    ]
p_1_tag = pemlMap [("p1", p_1)]
p_2_tag = pemlMap [("p2", p_2)]


-- nested expressions

singleton_tag_sequence, tag_sequence ∷ PemlExpr
singleton_tag_sequence = pemlSeq [p_1_tag]
tag_sequence = pemlSeq [p_1_tag, p_2_tag]

mixed_sequence ∷ PemlExpr
mixed_sequence = pemlSeq [ p_1_tag
                         , plain_seq
                         , p_2_tag
                         , p_true
                         , pemlMap [("nested tags", tag_sequence)]
                         ]

map_sequence ∷ PemlExpr
map_sequence = pemlSeq [ p_1_tag
                       , p_2_tag
                       , pemlMap [("nested tags", tag_sequence)]
                       ]

nested_tag_sequence ∷ PemlExpr
nested_tag_sequence = pemlSeq [ p_1_tag
                              , p_2_tag
                              , tag_sequence
                              ]

mixed_map ∷ PemlExpr
mixed_map = pemlMap
  [ ("kind", pemlTxt "api")
  , ("spec", pemlSeq
      [ pemlTag "port" $ pemlInt 8080
      , pemlTag "host" $ pemlMap
        [ ("name", pemlTxt "no.where")
        , ("real", pemlBln False)
        ]
      ])
  ]
