package com.faacets.operation.product

import spire.util.Opt

import com.faacets.core.Expr
import com.faacets.operation._

final class BoundedExprProductExtractor extends ProductExtractor[BoundedExpr] {

  def nParties(be: BoundedExpr) = be.expr.scenario.nParties

  def partialExtract(v: BoundedExpr): Opt[PolyProduct[BoundedExpr]] = {
    ProductExtractor[Expr].partialExtract(v.expr) match {
      case Opt(pp) => Opt(pp.map(expr => BoundedExpr(expr)))
      case _ => Opt.empty[PolyProduct[BoundedExpr]]
    }
  }

}
