package com.faacets.operation.product

import com.faacets.core.Expr
import com.faacets.operation._
import spire.util.Opt
import com.faacets.operation.instances.relabeling._

final class BellExpressionProductExtractor extends ProductExtractor[BellExpression] {

  def cwa: CanonicalWithAffineExtractor[BellExpression] = CanonicalWithAffineExtractor.forV[BellExpression]

  def partialExtract(v: BellExpression): Opt[PolyProduct[CanonicalDec[BellExpression]]] = {
    ProductExtractor[Expr].partialExtract(v.expr) match {
      case Opt(cde) => Opt(cde.map(_.map(expr => BellExpression(expr))))
      case _ => Opt.empty[PolyProduct[CanonicalDec[BellExpression]]]
    }
  }

}
