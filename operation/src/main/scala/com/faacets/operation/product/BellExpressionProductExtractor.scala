package com.faacets.operation.product

import com.faacets.core.Expr
import com.faacets.operation._
import spire.util.Opt
import com.faacets.operation.instances.relabeling._

final class BellExpressionProductExtractor extends ProductExtractor[BellExpression] {

  def nParties(be: BellExpression) = be.expr.scenario.nParties

  def partialExtract(v: BellExpression): Opt[PolyProduct[BellExpression]] = {
    ProductExtractor[Expr].partialExtract(v.expr) match {
      case Opt(pp) => Opt(pp.map(expr => BellExpression(expr)))
      case _ => Opt.empty[PolyProduct[BellExpression]]
    }
  }

}
