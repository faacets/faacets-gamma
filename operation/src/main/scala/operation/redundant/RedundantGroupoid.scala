package com.faacets
package operation
package redundant
/*
import spire.syntax.vectorSpace._
import spire.algebra.{Group, NullboxGroupoid, PartialAction, Semigroup, VectorSpace}
import spire.math.Rational
import spire.util.Nullbox

import qalg.immutable.QVector

import core._

final class RedundantGroupoid extends NullboxGroupoid[Redundant] {
  override def opIsDefined(x: Redundant, y: Redundant): Boolean =
    (x.expr.scenario == y.expr.scenario) && (x.expr.representation == y.expr.representation)
  def partialOp(x: Redundant, y: Redundant): Nullbox[Redundant] =
    if (!opIsDefined(x, y)) Nullbox.empty[Redundant] else {
      import x.expr.scenario.ExprVectorSpace
      Nullbox(Redundant(x.expr + y.expr))
    }
  def inverse(x: Redundant) = {
    import x.expr.scenario.ExprVectorSpace
    Redundant(-x.expr)
  }
}
*/