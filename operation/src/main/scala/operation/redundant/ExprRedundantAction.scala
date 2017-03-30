package com.faacets
package operation
package redundant
/*
import spire.syntax.vectorSpace._
import spire.algebra.{Group, Groupoid, NullboxPartialAction, Semigroup, VectorSpace}
import spire.math.Rational
import spire.util.Nullbox

import qalg.immutable.QVector

import core._

/** Reordering action on expressions.
  * 
  * Convention: right action add redundant terms, left action substracts them.
  */
final class ExprRedundantAction extends NullboxPartialAction[Expr, Redundant] {
  override def partialActl(r: Redundant, expr: Expr) =
    if (r.expr.scenario != expr.scenario) Nullbox.empty[Expr] else {
      implicit def vectorSpace = expr.scenario.ExprVectorSpace
      Nullbox(expr - r.expr)
    }

  def partialActr(expr: Expr, r: Redundant) =
    if (r.expr.scenario != expr.scenario) Nullbox.empty[Expr] else {
      implicit def vectorSpace = expr.scenario.ExprVectorSpace
      Nullbox(expr + r.expr)
    }
}
*/