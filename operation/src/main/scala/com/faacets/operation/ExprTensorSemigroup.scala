package com.faacets
package operation

import spire.algebra.Semigroup
import scalin.immutable.Vec
import scalin.immutable.dense._
import core._

/** Performs the tensor product of two Vecs. */
class ExprTensorSemigroup extends Semigroup[Expr] {

  def combine(x: Expr, y: Expr): Expr = {
    val zScenario = Scenario(x.scenario.parties ++ y.scenario.parties)
    val xSize = x.coefficients.length
    val ySize = y.coefficients.length
    val zSize = xSize * ySize
    val zCoefficients = Vec.tabulate(zSize) {
      case prodInd =>
        val xInd = prodInd % xSize
        val yInd = prodInd / xSize
        x.coefficients(xInd) * y.coefficients(yInd)
    }
    Expr(zScenario, zCoefficients)
  }

}
