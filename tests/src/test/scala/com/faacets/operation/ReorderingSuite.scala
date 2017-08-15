package com.faacets
package operation
package reordering

import spire.math.Rational
import scalin.immutable.Vec
import scalin.immutable.dense._

import com.faacets.core._

class ReorderingSuite extends FaacetsSuite {

  test("Simple reordering of a single party") {
    val source = "[(4 3 2)]".parseUnsafe[Scenario]
    val target = "[(3 4 2)]".parseUnsafe[Scenario]
    val reordering = Reordering(source, target)
    assert((source <|+|? reordering).get === target)
    assert(source.partialExtract[Reordering].isEmpty)
    val expr = DExpr(source, Vec[Rational](4,4,4,4,3,3,3,2,2))
    assert((expr <|+|? reordering).get.coefficients == Vec[Rational](3,3,3,4,4,4,4,2,2)) // TODO ===
  }

}
