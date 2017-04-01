package com.faacets
package operation
package instances

import com.faacets.core.{Expr, Relabeling}
import com.faacets.operation.relabeling.{VecRelabelingExtractor, VecRelabelingPartialAction}
import spire.algebra.Semigroup
import spire.algebra.partial.PartialAction

trait ExprInstances {

  implicit val operationExprSemigroup: Semigroup[Expr] = new ExprTensorSemigroup

  implicit val operationExprRelabelingPartialAction: PartialAction[Expr, Relabeling] =
    new VecRelabelingPartialAction[Expr]

  implicit val operationExprRelabelingExtractor: GroupOperationExtractor[Expr, Relabeling] =
    new VecRelabelingExtractor[Expr]

}
