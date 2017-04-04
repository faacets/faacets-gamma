package com.faacets.laws

import com.faacets.core.{Expr, PVec, PVecBuilder, Scenario}
import net.alasc.laws.{Cloner, Instances}
import org.scalacheck.{Arbitrary, Gen}
import scalin.immutable.Vec
import spire.math.Rational
import scalin.immutable.dense._

trait PVecs[V <: PVec[V]] {

  implicit def vecCloner(implicit builder: PVecBuilder[V]): Cloner[V] =
    Cloner((v: V) =>
      builder(
        Scenarios.scenarioCloner.make(v.scenario),
        Vec[Rational](v.coefficients.toIndexedSeq: _*)
      )
    )

}

object Exprs extends PVecs[Expr] {

  implicit val exprInstances: Instances[Expr] =
    Instances(Seq(
      Expr.correlators(Scenario.CHSH, Vec[Rational](0, 0, 0, 0, 1, 1, 0, 1, -1)),
      Expr(Scenario.nmk(1,1,2), Vec[Rational](-1, 1))
    ))

  def genExpr(scenario: Scenario): Gen[Expr] =
    Gen.buildableOfN[Seq[Rational], Rational](scenario.shapeNG.size, Rationals.genRational)
      .map( c => Expr.collinsGisin(scenario, Vec(c: _*)) )

  implicit def arbExpr(implicit arb: Arbitrary[Scenario]): Arbitrary[Expr] = Arbitrary( arb.arbitrary.flatMap( s => genExpr(s) ) )

}
