package com.faacets
package operation
package instances

import spire.algebra.Action
import spire.algebra.partial.PartialAction

import com.faacets.core.{Expr, Relabeling}
import com.faacets.data.Value
import com.faacets.operation.relabeling.{VecRelabelingExtractor, VecRelabelingPartialAction}

trait RelabelingInstances {

  implicit lazy val operationExprRelabelingPartialAction: PartialAction[Expr, Relabeling] =
    new VecRelabelingPartialAction[Expr]

  implicit lazy val operationExprRelabelingExtractor: GroupOperationExtractor[Expr, Relabeling] =
    new VecRelabelingExtractor[Expr]

  implicit val valueAction: Action[Value, Relabeling] = new Action[Value, Relabeling] {
    def actl(o: Relabeling, v: Value): Value = v
    def actr(v: Value, o: Relabeling): Value = v
  }

  implicit val boundedExprAction: PartialAction[BoundedExpr, Relabeling] =
    BoundedExpr.constructPartialAction[Relabeling](BoundedExpr.stdPreserved)

  /* TODO

    class VecRepresentativesSyntax[V <: Vec[V]](val v: V) extends AnyVal {
    def representatives(implicit ev: VecRepresentatives[V]) = ev.representatives(v)
  }

  implicit def exprBounds(expr: Expr) = new ExprBounds(expr)

  implicit val ExprSymmetricForms: SymmetricForms[Expr] = new VecSymmetricForms[Expr]
  implicit val CorrSymmetricForms: SymmetricForms[Corr] = new VecSymmetricForms[Corr]
  implicit def ExprRepresentatives = new VecRepresentatives[Expr]
  implicit def CorrRepresentatives = new VecRepresentatives[Corr]

   */
}
