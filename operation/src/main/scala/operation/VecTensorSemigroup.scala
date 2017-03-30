package com.faacets
package operation
/*
import spire.algebra.Semigroup
import spire.math.Rational
import qalg.immutable.QVector

import core._

/** Performs the tensor product of two Vecs */
class VecTensorSemigroup[V <: Vec[V]](implicit builder: VecBuilder[V]) extends Semigroup[V] {
  def op(x: V, y: V): V = {
    val representation = x.representation
    require(x.representation == y.representation)
    val zScenario = Scenario(x.scenario.parties ++ y.scenario.parties)
    val xSize = x.coefficients.length
    val ySize = y.coefficients.length
    val zSize = xSize * ySize
    val zCoefficients = QVector.tabulate(zSize) {
      case prodInd =>
        val xInd = prodInd % xSize
        val yInd = prodInd / xSize
        x.coefficients(xInd) * y.coefficients(yInd)
    }
    builder(zScenario, representation, zCoefficients)
  }
}
*/