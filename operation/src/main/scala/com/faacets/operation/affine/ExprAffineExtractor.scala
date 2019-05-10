package com.faacets
package operation
package affine

import spire.math.Rational
import spire.syntax.all._
import spire.util.Opt

import com.faacets.core._

final class ExprAffineExtractor[S <: Scenario with Singleton] extends GroupActionOperationExtractor[Expr[S], Affine] {

  def group = Affine.group

  def action = Affine.exprAction

  def computeShift(expr: Expr[S]): Rational = expr.inner(Behavior.uniformlyRandom(expr.scenario))

  def extractOperation(expr: Expr[S]): Opt[Affine] = {
    val shift = computeShift(expr)
    val withoutShift = expr - shift
    val factor = withoutShift.coefficients.gcd
    val affine = Affine(factor, shift)
    if (affine.isIdentity) Opt.empty[Affine] else Opt(affine.inverse)
  }

}
