package com.faacets
package operation
package affine
/*
import spire.syntax.all._
import spire.algebra.Group
import spire.math.Rational
import spire.util._

import net.alasc.syntax.sequence._

import polyta._

import core._

class ExprAffineExtractor extends GroupOperationExtractor[Expr, Affine] {
  def group = Affine.Group
  def action = Affine.ExprAction

  def computeShift(expr: Expr): Rational = expr.representation match {
    case NGRepresentation | SGRepresentation => expr.coefficients(0)
    case _ => expr.constant
  }
  def partialExtract(expr: Expr): Nullbox[Affine] = {
    val shift = computeShift(expr)
    val withoutShift = expr :- shift
    val factor = withoutShift.coefficients.commonFactor
    val affine = Affine(factor, shift)
    if (affine.isIdentity) Nullbox.empty[Affine] else Nullbox(affine.inverse)
  }
}
*/