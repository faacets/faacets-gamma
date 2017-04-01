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
import spire.syntax.eq._
import com.faacets.core.text._

trait GenExprBuilder[V <: GenExpr[V]] extends PVecBuilder[V] { self =>

  def zero(scenario: Scenario): V

  def one(scenario: Scenario): V

  def vectorSpace(scenario: Scenario): VectorSpace[V, Rational] = new VectorSpace[V, Rational] {

    def scalar = spire.math.Rational.RationalAlgebra

    def timesl(r: Rational, v: V): V = {
      require(v.scenario === scenario)
      r *: v
    }

    def negate(x: V): V = {
      require(x.scenario === scenario)
      -x
    }

    def zero: V = self.zero(scenario)

    def plus(x: V, y: V): V = {
      require(x.scenario === scenario)
      require(y.scenario === scenario)
      x + y
    }

  }

}

trait GenExpr[V <: GenExpr[V]] extends PVec[V] { lhs: V =>

  def builder: GenExprBuilder[V]

  /** Computes the inner product between this expression and the given behavior. */
  def inner(rhs: Behavior): Rational = lhs.coefficients.dot(rhs.coefficients)

  def :*(r: Rational): V = r *: lhs

  def *:(r: Rational): V = {
    if (r.isZero) builder.zero(scenario)
      else builder.updatedWithSymmetryGroup(lhs, scenario, coefficients * r, grp => Some(grp))
  }

  def unary_- : V = builder.updatedWithSymmetryGroup(lhs, scenario, -coefficients, grp => Some(grp))

  def +(rhs: V): V = {
    require(lhs.scenario === rhs.scenario)
    builder.apply(scenario, lhs.coefficients + rhs.coefficients)
  }

  def -(rhs: V): V = {
    require(lhs.scenario === rhs.scenario)
    builder.apply(scenario, lhs.coefficients - rhs.coefficients)
  }

  def +(rhs: Rational): V = lhs + rhs *: builder.one(lhs.scenario)

  def -(rhs: Rational): V = lhs - rhs *: builder.one(lhs.scenario)

}

/** Describes a Bell expression. */
class Expr protected (val scenario: Scenario, val coefficients: Vec[Rational]) extends NDVec[Expr] with GenExpr[Expr] { lhs =>

  def builder = Expr

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

object Expr extends NDVecBuilder[Expr] with GenExprBuilder[Expr] {

  implicit def builder: NDVecBuilder[Expr] with GenExprBuilder[Expr] = this

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

  def one(scenario: Scenario): Expr = {
    val ratio = Rational(SafeLong.one, scenario.nInputTuples)
    val pCoefficients = Vec.fillConstant(scenario.shapeP.size)(ratio)
    val res = applyUnsafe(scenario, pCoefficients)
    symmetryGroup(res)(scenario.group)
    res
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

  def averageNormalization(scenario: Scenario): Expr = one(scenario)

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
