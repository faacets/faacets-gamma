### A list of dependencies used in the Faacets project

... and their own dependencies, not listing scalatest/scalacheck/discipline used generally in tests:

- [alasc](http://github.com/denisrosset/alasc): metal, attributes, cyclo, shapeless, spire, fastparse
- [cats](http://github.com/typelevel/cats): compile/test dependencies
- [circe](http://github.com/circe/circe): cats-core (only dependency for circe-core); if we use the other modules, monocle (for optics) refined (for refined) shapeless (for generic, shapes) jawn (for parser)
- [circe-yaml](http://github.com/circe/circe-yaml): circe-core, circe-parser, snakeyaml
- [consolidate](http://github.com/denisrosset/consolidate): cats, shapeless
- [discipline](http://github.com/typelevel/discipline)
- [FastParse](http://github.com/lihaoyi/fastparse): lihaoyi/sourcecode (no further deps)-
- [gluon](http://github.com/denisrosset/gluon)
- [scalin](http://github.com/denisrosset/scalin): spire
- [shapeless](http://github.com/milessabin/shapeless): standard macro plugins
- [spire](http://github.com/non/spire): cats, algebra

#### Transitive dependencies

- [algebra](http://github.com/typelevel/algebra)
- [attributes](http://github.com/denisrosset/attributes)
- [cyclo](http://github.com/denisrosset/spire-cyclo)
- [metal](http://github.com/denisrosset/metal)
- [snakeyaml](http://www.snakeyaml.org)
