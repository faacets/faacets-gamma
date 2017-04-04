package com.faacets
package operation
package affine

import spire.syntax.all._
import spire.math.Rational
import spire.util.Opt


import core._

class ExprAffineExtractor extends GroupActionOperationExtractor[Expr, Affine] {

  def group = Affine.group

  def action = Affine.exprAction

  def computeShift(expr: Expr): Rational = expr.inner(Behavior.uniformlyRandom(expr.scenario))

  def partialExtract(expr: Expr): Opt[Affine] = {
    val shift = computeShift(expr)
    val withoutShift = expr - shift
    val factor = withoutShift.coefficients.gcd
    val affine = Affine(factor, shift)
    if (affine.isIdentity) Opt.empty[Affine] else Opt(affine.inverse)
  }

}
