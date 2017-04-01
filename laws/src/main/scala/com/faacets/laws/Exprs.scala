package com.faacets.laws

import com.faacets.core.{Expr, Scenario}
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import scalin.immutable.Vec
import spire.math.Rational
import scalin.immutable.dense._

object Exprs {


  def genExpr(scenario: Scenario): Gen[Expr] =
    Gen.buildableOfN[Seq[Rational], Rational](scenario.shapeNG.size, Rationals.genRational)
      .map( c => Expr.collinsGisin(scenario, Vec(c: _*)) )

  implicit def arbExpr(implicit arb: Arbitrary[Scenario]): Arbitrary[Expr] = Arbitrary( arb.arbitrary.flatMap( s => genExpr(s) ) )

}
