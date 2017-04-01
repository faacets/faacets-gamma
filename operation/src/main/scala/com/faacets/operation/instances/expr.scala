package com.faacets
package operation
package instances

import com.faacets.core.Expr
import spire.algebra.Semigroup

trait ExprInstances {

  implicit val ExprSemigroup: Semigroup[Expr] = new ExprTensorSemigroup

}
