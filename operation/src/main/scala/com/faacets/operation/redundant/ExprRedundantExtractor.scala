package com.faacets
package operation
package redundant
/*
import spire.syntax.vectorSpace._
import spire.algebra.{Group, Groupoid, PartialAction, Semigroup, VectorSpace}
import spire.math.Rational
import spire.util._

import qalg.immutable.QVector

import core._

final class ExprRedundantExtractor extends OperationExtractor[Expr, Redundant] {
  def action = Redundant.ExprAction
  def groupoid = Redundant.Groupoid

  def identity(expr: Expr) = Redundant(Expr(expr.scenario, expr.representation, QVector.fill(expr.coefficients.length)(Rational.zero)))

  override def canExtract(expr: Expr) = !expr.isInNonSignalingSubspace

  def partialExtract(expr: Expr): Nullbox[Redundant] = {
    val (nsig, sig) = expr.toNonSignaling
    if ((0 until sig.coefficients.length).forall(sig.coefficients(_) == 0))
      Nullbox.empty[Redundant]
    else {
      import sig.scenario.ExprVectorSpace
      Nullbox(Redundant(-sig))
    }
  }
}
*/