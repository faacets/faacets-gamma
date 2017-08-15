package com.faacets
package operation

import spire.math.SafeLong
import net.alasc.laws.Grps

import com.faacets.core._


class SymmetricFormsSuite extends FaacetsSuite {

  def testExpr(baseExpr: Expr, expectedPartiesOrder: SafeLong, symmetricForm: Expr => Expr): Unit = {
    forAll(Grps.genRandomElement(baseExpr.scenario.group)) { rl =>
      symmetricForm(baseExpr <|+| rl).symmetryGroup.partiesSubgroup.order shouldBe expectedPartiesOrder
    }
  }

  test("I3322") {
    testExpr(Expr.I3322, 2, (e: Expr) => e.symmetricRepresentative.extracted)
  }

  test("Sliwa4") {
    testExpr(Expr.Sliwa4, 2, (e: Expr) => e.symmetricRepresentative.extracted)
  }

  test("Sliwa7") {
    testExpr(Expr.Sliwa7, 6, (e: Expr) => e.symmetricRepresentative.extracted)
  }

  test("Sliwa10") {
    testExpr(Expr.Sliwa10, 3, (e: Expr) => e.cyclicRepresentative.extracted)
  }

}
