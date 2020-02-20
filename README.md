Plain Embedded Markup Language (PEML)
=====================================
> Programmable, type-safe, human-readable data description language.

PEML is a Haskell embedded domain-specific language (EDSL) that lets
you describe your data without visual clutter, using a lean, YAML-like
syntax. Just like in YAML and JSON, you build PEML expressions by
assembling literal values (strings, integers, etc.) into lists or
maps and then nest structures recursively at any depth---yep, it's
turtles all the way down. Here's what [example 2.3][yaml.ex-2.3]
from the [YAML][yaml] spec looks like in PEML

```haskell
ex_2_3 = do                  -- 'do' notation to define expressions
  "american" =: do           -- the tag operator (=:) names an expression
    (-:) "Boston Red Sox"    -- whereas (-:) introduces a list item 
    (-:) "Detroit Tigers"
    (-:) "New York Yankees"
  "national" =: do
    (-:) "New York Mets"
    (-:) "Chicago Cubs"
    (-:) "Atlanta Braves"
```

We support converting PEML to YAML and JSON, so you can easily dump
a PEML expression to a file as YAML or JSON. What the heck, why not
write plain YAML or JSON then?! Well, there are a couple of reasons
that might make PEML worth your while:

* **Type safety**. PEML piggybacks on Haskell's advanced type system
  to make sure you can only put together expressions that translate
  to valid YAML/JSON. As an added bonus, you can stop worrying about
  subtle white-space/indentation issues and concentrate on describing
  your data instead. But you could push the envelope and, e.g. encode
  your config rules in the type system so you get to know about any
  blunder through compiler errors rather than a prod outage. Likewise
  you get the freedom to refactor your config mercilessly as the
  compiler safely guides you through the process.
* **Programmability**. After having had truckloads of YAML gently
  shovelled down your throat with a dozer, you start wondering how to
  abstract away bits and pieces that look similar but aren't exactly
  the same. Or how to factor out common patterns in a lib to reuse
  across projects. Or how to modularise stuff to minimise the impact
  of change. Or...
  Ah! You need "programmable YAML"! While code generators and template
  languages may be a good fit for your use case, they typically clip
  your wings: you can only write a subset of all possible programs that
  would be useful. Reaching for time-honoured C&P tech, scripts and
  all that to get around roadblocks usually leaves you with a hodgepodge
  nobody dares even looking at. Why struggle? A PEML expression is
  Haskell data living in a Haskell program which means you can do pretty
  much whatever you like with it---from defining your own functions to
  fancy AST transformations, it's all at your fingertips. Also Haskell
  is a real productivity booster with a best-in-class approach to
  abstraction, modularity, reuse, and conciseness and comes with a
  ton of top-notch libs to help you get where you want quick-quick,
  chop-chop.




[yaml]: https://yaml.org/spec/1.2/spec.html
    "YAML v1.2"
[yaml.ex-2.3]: https://yaml.org/spec/1.2/spec.html#id2759963
    "YAML v1.2 - 2.1. Collections"
