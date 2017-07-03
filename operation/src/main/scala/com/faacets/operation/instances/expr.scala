package com.faacets
package operation
package instances

import com.faacets.core.{Expr, Relabeling}
import com.faacets.operation.relabeling.{VecRelabelingExtractor, VecRelabelingPartialAction}
import spire.algebra.Semigroup
import spire.algebra.partial.PartialAction

trait ExprInstances {

  implicit val operationExprRelabelingPartialAction: PartialAction[Expr, Relabeling] =
    new VecRelabelingPartialAction[Expr]

  implicit val operationExprRelabelingExtractor: GroupOperationExtractor[Expr, Relabeling] =
    new VecRelabelingExtractor[Expr]

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
