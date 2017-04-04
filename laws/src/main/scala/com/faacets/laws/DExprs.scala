package com.faacets
package laws

import com.faacets.core.{DExpr, Expr, Scenario}
import org.scalacheck._
import spire.math.Rational
import scalin.immutable.dense._

object DExprs extends PVecs[DExpr] {

  def genPureSignaling(scenario: Scenario): Gen[DExpr] = {
    val dexprs = DExpr.nonSignalingSubspaceTests(scenario)
    Gen.containerOfN[Seq, Rational](dexprs.size, Rationals.genRational).map { coeffs =>
      (coeffs zip dexprs).foldLeft(DExpr.zero(scenario)) {
        case (acc, (c, d)) => acc + c *: d
      }
    }
  }

  def genDExpr(expr: Expr): Gen[DExpr] = genPureSignaling(expr.scenario).map( d => DExpr(d.scenario, expr.coefficients + d.coefficients) )

}
