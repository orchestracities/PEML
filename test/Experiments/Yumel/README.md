Yumel
=====
> PEML's oupa.


This is how PEML started: exploring the design space for a YAML
combinator lib in Haskell. I'm keeping this code around for now
since it could come in handy later on if I decide to tackle the
problem of extending the PEML builder EDSL to be able to build
lists of tags too or even any PEML expression while still using
only two combinators---i.e. sequence `-:` and tag `=:` operators.
In fact, at the moment the EDSL can only build a subset of PEML.

Now, the Yumel AST is a plain old ADT and so is much easier to
handle than PEML's. The `Yumel.Lang.Builder` module tries to
build the whole of Yumel with just two combinators and so it
could be a good starting point to get ideas on how to tackle
the same question in PEML. In particular, I tried overloading
those two combinators (using data/type families) so that they'd
do different things in sequence and map contexts, respectively,
but failed miserably---to the type whizzes out there, this is a
cry for help!
