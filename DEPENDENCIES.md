### A list of dependencies used in the Faacets project

... and their own dependencies, not listing scalatest/scalacheck/discipline used generally in tests:

- [alasc](github.com/denisrosset/alasc): metal, attributes, cyclo, shapeless, spire, fastparse
- [cats](github.com/typelevel/cats): compile/test dependencies
- [circe](github.com/circe/circe): cats-core (only dependency for circe-core); if we use the other modules, monocle (for optics) refined (for refined) shapeless (for generic, shapes) jawn (for parser)
- [circe-yaml](github.com/circe-yaml): circe-core, circe-parser, snakeyaml
- [consolidate](github.com/denisrosset/consolidate): cats, shapeless
- [discipline](github.com/typelevel/discipline)
- [FastParse](github.com/lihaoyi/fastparse): lihaoyi/sourcecode (no further deps)-
- [scalin](github.com/denisrosset/scalin): spire
- [shapeless](github.com/milessabin/shapeless): standard macro plugins
- [spire](github.com/non/spire): cats, algebra

#### Transitive dependencies

- [algebra](github.com/typelevel/algebra)
- [attributes](github.com/denisrosset/attributes)
- [cyclo](github.com/denisrosset/spire-cyclo)
- [metal](github.com/denisrosset/metal)
- [snakeyaml](www.snakeyaml.org)
