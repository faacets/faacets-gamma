package com.faacets
package laws

import spire.math.Rational
import scalin.immutable.dense._

import org.scalacheck._

import com.faacets.core.{DExpr, Expr, Scenario}

object DExprs extends PVecs[DExpr] {

  def genPureSignaling(scenario: Scenario): Gen[DExpr[scenario.type]] = {
    val dexprs = DExpr.nonSignalingSubspaceTests(scenario)
    Gen.containerOfN[Seq, Rational](dexprs.size, Rationals.genRational).map { coeffs =>
      (coeffs zip dexprs).foldLeft(DExpr.zero(scenario)) {
        case (acc, (c, d)) => acc + c *: d
      }
    }
  }

  def genDExpr[S <: Scenario with Singleton](expr: Expr[S]): Gen[DExpr[S]] = genPureSignaling(expr.scenario: S).map( d => DExpr(d.scenario: S, expr.coefficients + d.coefficients) )

}
