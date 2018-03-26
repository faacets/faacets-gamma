package com.faacets.laws

import spire.math.Rational
import scalin.immutable.Vec
import scalin.immutable.dense._
import net.alasc.laws.{Cloner, Instances}

import org.scalacheck.{Arbitrary, Gen}

import com.faacets.core.{Expr, PVec, PVecBuilder, Scenario}

trait PVecs[V[X <: Scenario with Singleton] <: PVec[V, X]] {

  implicit def vecCloner[S <: Scenario with Singleton](implicit builder: PVecBuilder[V]): Cloner[V[S]] =
    Cloner((v: V[S]) =>
      builder(
        v.scenario: S,
        Vec[Rational](v.coefficients.toIndexedSeq: _*)
      )
    )

}

object Exprs extends PVecs[Expr] {

  implicit val exprInstances: Instances[Expr[_]] =
    Instances(Seq(
      Expr.correlators(Scenario.CHSH, Vec[Rational](0, 0, 0, 0, 1, 1, 0, 1, -1)),
      Expr(Scenario.nmk(1,1,2), Vec[Rational](-1, 1))
    ))

  def genExpr(scenario: Scenario): Gen[Expr[scenario.type]] =
    Gen.buildableOfN[Seq[Rational], Rational](scenario.shapeNG.size, Rationals.genRational)
      .map( c => Expr.collinsGisin(scenario, Vec(c: _*)) )

  implicit def arbExpr[S <: Scenario with Singleton](w: shapeless.Witness.Aux[S]): Arbitrary[Expr[S]] =
    Arbitrary(genExpr(w.value: S))
}
