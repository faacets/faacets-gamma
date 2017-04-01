package com.faacets
package operation
/*
import spire.math.Rational
import spire.syntax.ring._

import net.alasc.syntax.sequence._
import polyta._

import core._
import relabeling._

class ExprBounds(val expr: Expr) extends AnyVal {

  // TODO: use linear programming
  def noSignalingBounds: (Rational, Rational) = {
    val families = NonSignalingPolytope(expr.scenario, true).vertices
    var minimum = Vec.inner(expr, families.head)
    var maximum = minimum
    for (f <- families; r <- f.representatives.iterator) {
      val value = Vec.inner(expr, r.get)
      if (value < minimum)
        minimum = value
      if (value > maximum)
        maximum = value
    }
    (minimum, maximum)
  }

  /**
    * Tests whether the current `Expr` is a facet of the local polytope.
    * 
    * Extremal points that saturate the expression are enumerated; if it is a facet, 
    * the span of these points should be the affine non-signaling space
    * minus one dimension.
    */
  def localBoundsAndLocalFacets: ((Rational, Boolean), (Rational, Boolean)) = {
    val extPoints = expr.scenario.matrices.corr(expr.representation, NPRepresentation) *
    expr.scenario.matrices.corr(NPRepresentation, WRepresentation)
    val extPointsRank = extPoints.rank
    val values = extPoints.t * expr.coefficients
    val lowerLB = values.toIndexedSeq.min
    val upperLB = values.toIndexedSeq.max
    def extPointsWithValueFullRank(bound: Rational) = {
      val indices = (0 until extPoints.cols).filter(values(_) == bound)
      val E = qalg.mutable.QMatrix.zeros(extPoints.rows, indices.length)
      for ((epc, ec) <- indices.view.zipWithIndex)
        E(::, ec) = extPoints(::, epc)
      E.rank == extPointsRank - 1
    }
    ((lowerLB, extPointsWithValueFullRank(lowerLB)), (upperLB, extPointsWithValueFullRank(upperLB)))
  }

  def localBounds: (Rational, Rational) = {
    val extPoints = expr.scenario.matrices.corr(expr.representation, NPRepresentation) *
    expr.scenario.matrices.corr(NPRepresentation, WRepresentation)
    val values = extPoints.t * expr.coefficients
    (values.toIndexedSeq.min, values.toIndexedSeq.max)
  }
}
*/