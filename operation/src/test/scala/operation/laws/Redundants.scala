package com.faacets
package operation
package laws
/*
import scala.annotation.tailrec

import org.scalacheck._

import spire.math.Rational
import spire.syntax.action._

import net.alasc.math.{Domain, Perm}
import net.alasc.laws._
import net.alasc.std.seq._

import qalg.immutable.QVector

import core._
import core.perm._
import core.repr.{NCTerm, SCTerm}
import core.laws.OperationGenerator
import core.laws.Exprs.exprCloner

object Redundants {
  import Rationals._

  def sigIndicesSC(scenario: Scenario): Seq[Int] = {
    val terms = SCTerm.all(scenario)

    (0 until scenario.shapeSC.size).filterNot(terms(_).isInstanceOf[repr.NCTerm])
  }

  def genRedundantExprSC(scenario: Scenario): Gen[Expr] = {
    val indices = sigIndicesSC(scenario)
    Gen.containerOfN[Seq, Rational](indices.length, genRational).map { sigCoeffs =>
      val indexMap = (indices zip sigCoeffs).toMap
      val coeffs =
        QVector.tabulate(scenario.shapeSC.size)( indexMap.getOrElse(_, Rational.zero) )
      Expr(scenario, SCRepresentation, coeffs)
    }
  }

  def genRedundant(scenario: Scenario): Gen[Redundant] = for {
    expr <- genRedundantExprSC(scenario)
    repr <- core.laws.Representations.genSignaling
  } yield Redundant(expr.to(repr))

  implicit def arbRedundant(implicit arbScenario: Arbitrary[Scenario]): Arbitrary[Redundant] =
    Arbitrary(arbScenario.arbitrary.flatMap(genRedundant(_)))

  def genRedundant(expr: Expr): Gen[Redundant] =
    if (expr.representation.isSignaling)
      genRedundantExprSC(expr.scenario)
        .map(_.to(expr.representation)).map(Redundant(_))
    else Gen.fail[Redundant]

  implicit val redundantGenerator: OperationGenerator[Expr, Redundant] = OperationGenerator[Expr, Redundant](expr => genRedundant(expr))

  implicit val redundantCloner: Cloner[Redundant] =
    Cloner((r: Redundant) => Redundant(exprCloner.make(r.expr)))

  implicit val redundantInstances: Instances[Redundant] =
    Instances(Seq(
      Redundant(Expr(Scenario.nmk(1,2,2), SCRepresentation, QVector(0, 0, 0, 1))),
      Redundant(Expr(Scenario.nmk(1,2,2), SCRepresentation, QVector(0, 0, 0, 0)))
    ))
}
*/