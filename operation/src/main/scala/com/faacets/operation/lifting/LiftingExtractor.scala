package com.faacets
package operation
package lifting

import spire.util._

import data._

import core._

final class ExprLiftingExtractor extends OperationExtractor[Expr, Lifting] {

  implicit def action = Lifting.exprAction

  implicit def groupoid = Lifting.groupoid

  def identity(expr: Expr) = {
    val grouping = Grouping(expr)
    Lifting(grouping, grouping)
  }

  def partialExtract(expr: Expr): Opt[Lifting] = {
    val grouping = Grouping(expr)
    if (grouping.isLifting) Opt(Lifting(grouping, Grouping.noLifting(grouping.minimalScenario)))
    else Opt.empty[Lifting]
  }

}
