### A list of dependencies used in the Faacets project

... and their own dependencies, not listing scalatest/scalacheck/discipline used generally in tests:

- [Spire](github.com/non/spire): cats, [algebra](github.com/typelevel/algebra)
- [Scalin](github.com/denisrosset/scalin): spire
- [FastParse](github.com/lihaoyi/fastparse): lihaoyi/sourcecode (no further deps)
- [Cats](github.com/typelevel/cats): compile/test dependencies
- [Circe](github.com/circe/circe): cats-core (only dependency for circe-core); if we use the other modules, monocle (for optics) refined (for refined) shapeless (for generic, shapes) jawn (for parser)
