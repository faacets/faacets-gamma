package com.faacets
package core

import spire.math.{Rational, SafeLong}
import spire.algebra.VectorSpace
import com.faacets.core.repr.ReverseKronHelpers.revKronMatVec
import scalin.immutable.dense._
import scalin.immutable.{Mat, Vec}

import scala.reflect.classTag
import cats.data.{Validated, ValidatedNel}
import com.faacets.core.NDVec.attributes.symmetryGroup
import spire.syntax.cfor._
import spire.syntax.eq._
import scalin.syntax.all._
import cats.syntax.traverse._
import cats.instances.vector._
import com.faacets.core.text._

import scala.collection.mutable.ArrayBuffer

trait GenExpr extends PVec { expr =>

  /** Computes the inner product between this expression and the given behavior. */
  def inner(corr: Behavior): Rational = expr.coefficients.dot(corr.coefficients)

}

/** Describes a Bell expression. */
class Expr protected (val scenario: Scenario, val coefficients: Vec[Rational]) extends NDVec with GenExpr { lhs =>

  type V = Expr

  def prefix = "Expr"

  def classTagV = classTag[Expr]

  def toDExpr: DExpr = DExpr(scenario, coefficients)

  def inBasis(matChoice: Party => Mat[Rational]): Vec[Rational] =
    revKronMatVec(scenario.parties.map(p => matChoice(p).t), coefficients)

  def correlators: Vec[Rational] = inBasis(p => p.matrices.matSPfromSC * p.matrices.matSCfromNC)

  def collinsGisin: Vec[Rational] = inBasis(p => p.matrices.matSPfromSG * p.matrices.matSGfromNG)

  def fullTable: Table = toDExpr.fullTable

  def collinsGisinTable: Table =
    if (scenario.nParties == 1) Table(collinsGisin.toRowMat)
    else if (scenario.nParties == 2) Table(collinsGisin.reshape(scenario.parties(0).shapeNG.size, scenario.parties(1).shapeNG.size).t)
    else sys.error("Scenarios with > 2 parties are not supported")

  def correlatorsTable: Table =
    if (scenario.maxNumOutputs > 2) sys.error("Scenarios with > 2 outputs are not supported")
    else if (scenario.nParties == 1) Table(correlators.toRowMat)
    else if (scenario.nParties == 2) Table(correlators.reshape(scenario.parties(0).shapeNC.size, scenario.parties(1).shapeNC.size).t)
    else sys.error("Scenarios with > 2 parties are not supported")

  def *:(r: Rational): Expr = {
    if (r.isZero) Expr.zero(scenario) else {
      val res = Expr.applyUnsafe(scenario, coefficients * r)
      NDVec.attributes.symmetryGroup.get(lhs)(NDVec.attributes.symmetryGroup.forNDVec) match {
        case Some(grp) => NDVec.attributes.symmetryGroup(res)(grp)
        case None =>
      }
      res
    }
  }

  def unary_- : Expr = {
    val res = Expr.applyUnsafe(scenario, -coefficients)
    NDVec.attributes.symmetryGroup.get(lhs) match {
      case Some(grp) => NDVec.attributes.symmetryGroup(res)(grp)
      case None =>
    }
    res
  }

  def +(rhs: Expr): Expr = {
    require(lhs.scenario === rhs.scenario)
    Expr.applyUnsafe(scenario, lhs.coefficients + rhs.coefficients)
  }

  def fullExpression: String = toDExpr.fullExpression

  def collinsGisinExpression: String = {
    val cgCoeffs = collinsGisin
    val coeffTerms = (0 until cgCoeffs.length).filterNot(i => cgCoeffs(i).isZero).map { ind =>
      val (kk, xx) = scenario.ind2subNG(ind)
      (cgCoeffs(ind), CGTerm.fromKX(kk, xx))
    }
    Term.printExpression(coeffTerms)
  }

  def correlatorsExpression: String = {
    require(scenario.maxNumOutputs <= 2, "Scenario must have binary outputs to use the correlators expression")
    val cCoeffs = correlators
    val coeffTerms = (0 until cCoeffs.length).filterNot(i => cCoeffs(i).isZero).map { ind =>
      val (kk, xx) = scenario.ind2subNC(ind)
      (cCoeffs(ind), CorrelatorsTerm.fromKX(kk, xx))
    }
    Term.printExpression(coeffTerms)
  }

}

object Expr extends NDVecBuilder[Expr] {

  def parseExpression(scenario: Scenario, expression: String): ValidatedNel[String, Expr] =
  DExpr.parseExpression(scenario, expression) andThen { dExpr =>
    val expr = dExpr.projected
    if (dExpr.coefficients == expr.coefficients) // TODO use ===
      Validated.valid(expr)
    else
      Validated.invalidNel("Expression is not in nonsignaling subspace")
  }

  import DExpr.changeBasis

  def zero(scenario: Scenario): Expr = {
    val res = applyUnsafe(scenario, Vec.fillConstant(scenario.shapeP.size)(Rational.zero))
    symmetryGroup(res)(scenario.group)
    res
  }

  def vectorSpace(scenario: Scenario): VectorSpace[Expr, Rational] = new VectorSpace[Expr, Rational] {
    def scalar = spire.math.Rational.RationalAlgebra
    def timesl(r: Rational, v: Expr): Expr = r *: v
    def negate(x: Expr): Expr = -x
    def zero: Expr = Expr.zero(scenario)
    def plus(x: Expr, y: Expr): Expr = (x + y)
  }

  def constant(scenario: Scenario): Expr = {
    val ratio = Rational(SafeLong.one, scenario.nInputTuples)
    val pCoefficients = Vec.fillConstant(scenario.shapeP.size)(ratio)
    apply(scenario, pCoefficients) // TODO use applyUnsafe
  }

  def inNonSignalingSubspace(scenario: Scenario, coefficients: Vec[Rational]): Boolean = {
    val pCoefficients = changeBasis(scenario, p => p.matrices.matProjectionInSP, coefficients)
    coefficients == pCoefficients // TODO: Eq[Vec[Rational]]
  }

  def apply(scenario0: Scenario, coefficients0: Vec[Rational]): Expr = {
    require(inNonSignalingSubspace(scenario0, coefficients0), "Coefficients are not in nonsignaling subspace")
    applyUnsafe(scenario0, coefficients0)
  }

  def applyUnsafe(scenario: Scenario, coefficients: Vec[Rational]): Expr = {
    require(coefficients.length == scenario.shapeP.size, "Coefficients vector length is incorrect")
    new Expr(scenario, coefficients)
  }

  def collinsGisin(scenario: Scenario, collinsGisinCoefficients: Vec[Rational]): Expr = {
    val pCoefficients = changeBasis(scenario, p => p.matrices.matNGfromSG * p.matrices.matSGfromSP, collinsGisinCoefficients)
    applyUnsafe(scenario, pCoefficients)
  }

  def correlators(scenario: Scenario, correlatorsCoefficients: Vec[Rational]): Expr = {
    val pCoefficients = changeBasis(scenario, p => p.matrices.matNCfromSC * p.matrices.matSCfromSP, correlatorsCoefficients)
    applyUnsafe(scenario, pCoefficients)
  }

  def averageNormalization(scenario: Scenario): Expr = {
    val pCoefficients =  Vec.fillConstant(scenario.shapeP.size)(Rational(1, scenario.nInputTuples))
    applyUnsafe(scenario, pCoefficients)
  }

  def CHSH = correlators(Scenario.CHSH, Vec[Rational](0, 0, 0, 0, 1, 1, 0, 1, -1))

  def Sliwa10 = correlators(Scenario.nmk(3, 2, 2),
    Vec[Rational](4, 0, 0, 0, -1, -1, 0, -1, -1, 0, -1, 1, -1, -1, 0, 1, 0, 1, 0, -1, 1, 1, 0, -1, -1, 1, 0))

  def Sliwa10Sym = correlators(Scenario.nmk(3, 2, 2),
    Vec[Rational](4, 0, 0, 0, -1, -1, 0, 1, 1, 0, -1, 1, -1, -1, 0, -1, 0, -1, 0, -1, 1, 1, 0, -1, 1, -1, 0))

  def Sliwa7 = correlators(Scenario.nmk(3, 2, 2),
    Vec[Rational](4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -3, -1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1))

  def Sliwa4 = correlators(Scenario.nmk(3, 2, 2),
    Vec[Rational](2, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1, 0))

  def I3322 = collinsGisin(Scenario.nmk(2, 3, 2),
    Vec[Rational](0, -1, 0, 0, -2, 1, 1, 1, -1, 1, 1, -1, 0, 1, -1, 0))

}
