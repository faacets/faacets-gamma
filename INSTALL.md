## How to install/run Faacets locally

[TODO]

### How to compile and test locally the documentation website

We use [sbt-microsites](https://47deg.github.io/sbt-microsites/) to write and publish the documentation.
The following steps show how to compile and test the website locally under Ubuntu.
It should work also under other Linux distributions and macOS.

#### Prerequisites

Install:

- [SBT](http://www.scala-sbt.org/), the Scala build tool
- [Jekyll](https://jekyllrb.com/), a static website builder

#### Steps

First, check that you can run SBT, that SBT downloaded all required packages, and that the Faacets code runs (run `sbt test` from the command line).

1) Compile the documentation: run `sbt docs/makeMicrosite`.
2) After successful compilation, go to the `docs/target/jekyll` subdirectory, run `jekyll serve --baseurl /faacets`
3) Go to [http://127.0.0.1:4000/faacets/] with your web browser. Don't forget the trailing slash.

