package com.faacets
package core

import com.faacets.laws.Relabelings
import spire.syntax.action._
import spire.syntax.group._
import spire.math.Rational
import net.alasc.syntax.all._
import scalin.immutable.dense._
import scalin.immutable.Vec


class VecSymmetryGroupCheck extends FaacetsSuite {
  val scenario = "[(2 2) (3 3 3) (2 2)]".parseUnsafe[Scenario]
  val barnea = Expr.collinsGisin(scenario,
      Vec[Rational](-1,1,-1,1,-1,1,-1,1,-1,0,0,0,-2,-2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,-2,0,0,-2,0,0,2,0,0,0,0,0,4,0,0,0,0,0,0,0,0))

  test("Symmetry group is preserved correctly after relabeling") {
    forAll(Relabelings.genRelabeling(scenario), Relabelings.genRelabeling(scenario)) { (r1, r2) =>

      val b1 = barnea <|+| r1

      b1.symmetryGroup // force computation
      assert(NDVec.attributes.symmetryGroup.get(b1).nonEmpty) // symmetry group should have been computed
      val b1clone = Expr(scenario, b1.coefficients)
      assert(b1.symmetryGroup === b1clone.symmetryGroup)

      val b2 = b1 <|+| r2
      val b2bis = b1clone <|+| r2
      val b2clone = Expr(scenario, b2.coefficients)
      assert(b2.symmetryGroup === b2bis.symmetryGroup)
      assert(b2.symmetryGroup === b2clone.symmetryGroup)
    }
  }

}
