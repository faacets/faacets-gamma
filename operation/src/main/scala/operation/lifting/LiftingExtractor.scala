package com.faacets
package operation
package lifting
/*
import spire.util._

import net.alasc.math.{Grp, Perm, Domain}

import data._

import core._
import core.perm.{Relabeling, PartyRelabeling}

final class ExprLiftingExtractor extends OperationExtractor[Expr, Lifting] {
  implicit def action = Lifting.ExprAction
  implicit def groupoid = Lifting.Groupoid
  def identity(expr: Expr) = {
    val grouping = Grouping(expr)
    Lifting(grouping, grouping)
  }
  def partialExtract(expr: Expr): Nullbox[Lifting] = {
    val grouping = Grouping(expr)
    if (grouping.isLifting) Nullbox(Lifting(grouping, grouping.compact)) else Nullbox.empty[Lifting]
  }
}
*/