package com.faacets
package operation
/*
import org.scalatest.FunSuite

import spire.syntax.action._
import spire.syntax.group._
import spire.math.Rational

import net.alasc.syntax.all._

import qalg.immutable.QVector

import data._
import core._

class VecSymmetryGroupCheck extends FunSuite {
  val scenario = "[(2 2) (3 3 3) (2 2)]".fromText[Scenario]
  val barnea = Expr(scenario, NGRepresentation,
      QVector(-1,1,-1,1,-1,1,-1,1,-1,0,0,0,-2,-2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,-2,0,0,-2,0,0,2,0,0,0,0,0,4,0,0,0,0,0,0,0,0))

  test("Symmetry group is preserved correctly after relabeling") {
    for (i <- 0 until 20) {
      val r1 = scenario.group.randomElement(scala.util.Random)
      val r2 = scenario.group.randomElement(scala.util.Random)

      barnea.symmetryGroup.chain
      val b1 = barnea <|+| r1
      assert(b1.symmetryGroupIfComputed.nonEmpty)
      val b1clone = Expr(scenario, NGRepresentation, b1.coefficients)
      assert(b1.symmetryGroup == b1clone.symmetryGroup)

      val b2 = b1 <|+| r2
      val b2bis = b1clone <|+| r2
      val b2clone = Expr(scenario, NGRepresentation, b2.coefficients)
      assert(b2.symmetryGroup == b2bis.symmetryGroup)
      assert(b2.symmetryGroup == b2clone.symmetryGroup)
    }
  }
}
*/