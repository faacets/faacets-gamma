package com.faacets
package core

import cats.data.Validated.Valid
import cats.data.{Validated, ValidatedNel}
import spire.math.{Rational, SafeLong}
import scalin.immutable.dense._
import scalin.immutable.{Mat, Vec}
import com.faacets.core.NDVec.attributes.symmetryGroup
import com.faacets.core.repr.ReverseKronHelpers.revKronMatVec
import com.faacets.core.text.UserVecRational.userVecRationalTextable
import com.faacets.core.text._
import spire.algebra.Action

/** Describes a Bell expression in the relevant nonsignaling subspace. */
class Expr[S <: Scenario with Singleton] protected (val scenario: S, val coefficients: Vec[Rational]) extends NDVec[Expr, S] with GenExpr[Expr, S] { lhs =>

  def builder = Expr

  def prefix = "Expr"

  def toDExpr: DExpr[S] = DExpr(scenario: S, coefficients)

  def inBasis(matChoice: Party => Mat[Rational]): Vec[Rational] =
    revKronMatVec(scenario.parties.map(p => matChoice(p).t), coefficients)

  def correlators: Vec[Rational] = inBasis(p => p.matrices.matSPfromSC * p.matrices.matSCfromNC)

  def collinsGisin: Vec[Rational] = inBasis(p => p.matrices.matSPfromSG * p.matrices.matSGfromNG)

  def fullTable_BA: Table = toDExpr.fullTable_BA

  def collinsGisinTable_BA: Table =
    if (scenario.nParties == 1) Table(collinsGisin.toRowMat)
    else if (scenario.nParties == 2) Table(collinsGisin.reshape(scenario.parties(0).shapeNG.size, scenario.parties(1).shapeNG.size).t)
    else sys.error("Scenarios with > 2 parties are not supported")

  def correlatorsTable_BA: Table =
    if (scenario.maxNumOutputs > 2) sys.error("Scenarios with > 2 outputs are not supported")
    else if (scenario.nParties == 1) Table(correlators.toRowMat)
    else if (scenario.nParties == 2) Table(correlators.reshape(scenario.parties(0).shapeNC.size, scenario.parties(1).shapeNC.size).t)
    else sys.error("Scenarios with > 2 parties are not supported")

  def expression: String = toDExpr.expression

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

  implicit def exprRelabelingAction[S <: Scenario with Singleton]: Action[Expr[S], Relabeling.Aux[S]] =
    new VecRelabelingAction[Expr, S]

  def parseExpression(scenario: Scenario, expression: String): ValidatedNel[String, Expr[scenario.type]] =
  DExpr.parseExpression(scenario, expression) andThen { dExpr =>
    val expr = dExpr.projected
    if (dExpr.coefficients == expr.coefficients) // TODO use ===
      Validated.valid(expr)
    else
      Validated.invalidNel("Expression is not in nonsignaling subspace")
  }

  import DExpr.changeBasis

  def zero(scenario: Scenario): Expr[scenario.type] = {
    val res = applyUnsafe(scenario, Vec.fillConstant(scenario.shapeP.size)(Rational.zero))
    symmetryGroup(res)(scenario.group)
    res
  }

  def one(scenario: Scenario): Expr[scenario.type] = {
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

  def apply(scenario: Scenario, coefficients: Vec[Rational]): Expr[scenario.type] =
    validate[scenario.type](scenario, coefficients).fold(ls => throw new IllegalArgumentException(ls.toList.mkString("\n")), identity)

  def applyUnsafe(scenario: Scenario, coefficients: Vec[Rational]): Expr[scenario.type] = {
    require(coefficients.length == scenario.shapeP.size, "Coefficients vector length is incorrect")
    new Expr(scenario, coefficients)
  }

  def validateCollinsGisin(scenario: Scenario, collinsGisinCoefficients: Vec[Rational]): ValidatedNel[String, Expr[scenario.type]] =
    if (collinsGisinCoefficients.length == scenario.shapeNG.size)
      Valid(collinsGisin(scenario, collinsGisinCoefficients))
    else
      Validated.invalidNel("Coefficients vector length is incorrect")

  def validateCorrelators(scenario: Scenario, correlatorsCoefficients: Vec[Rational]): ValidatedNel[String, Expr[scenario.type]] =
    if (correlatorsCoefficients.length == scenario.shapeNC.size)
      Valid(correlators(scenario, correlatorsCoefficients))
    else
      Validated.invalidNel("Coefficients vector length is incorrect")

  def collinsGisin(scenario: Scenario, collinsGisinCoefficients: Vec[Rational]): Expr[scenario.type] = {
    val pCoefficients = changeBasis(scenario, p => p.matrices.matNGfromSG * p.matrices.matSGfromSP, collinsGisinCoefficients)
    applyUnsafe(scenario, pCoefficients)
  }

  def correlators(scenario: Scenario, correlatorsCoefficients: Vec[Rational]): Expr[scenario.type] = {
    val pCoefficients = changeBasis(scenario, p => p.matrices.matNCfromSC * p.matrices.matSCfromSP, correlatorsCoefficients)
    applyUnsafe(scenario, pCoefficients)
  }

  def parseCollinsGisinVector(scenario: Scenario, coefficientsString: String): ValidatedNel[String, Expr[scenario.type]] =
    userVecRationalTextable.fromText(coefficientsString) andThen { coeffs =>
      if (coeffs.length != scenario.shapeNG.size)
        Validated.invalidNel(s"Incorrect coefficient vector length ${coeffs.length}, should be ${scenario.shapeNG.size}")
      else
        Validated.valid(Expr.collinsGisin(scenario, coeffs))
    }

  def parseCorrelatorsVector(scenario: Scenario, coefficientsString: String): ValidatedNel[String, Expr[scenario.type]] =
    userVecRationalTextable.fromText(coefficientsString) andThen { coeffs =>
      if (scenario.minNumOutputs < 2 || scenario.maxNumOutputs > 2)
        Validated.invalidNel(s"Correlators are only defined for scenarios with binary outputs")
      else if (coeffs.length != scenario.shapeNC.size)
        Validated.invalidNel(s"Incorrect coefficient vector length ${coeffs.length} should be ${scenario.shapeNG.size}")
      else
        Validated.valid(Expr.correlators(scenario, coeffs))
    }

  def parseVector(scenario: Scenario, coefficientsString: String): ValidatedNel[String, Expr[scenario.type]] =
    userVecRationalTextable.fromText(coefficientsString) andThen { coeffs => Expr.validate[scenario.type](scenario, coeffs, None) }

  def averageNormalization(scenario: Scenario): Expr[scenario.type] = one(scenario)

  val CHSH: Expr[Scenario.CHSH.type] = correlators(Scenario.CHSH, Vec[Rational](0, 0, 0, 0, 1, 1, 0, 1, -1))

  val Sliwa10: Expr[Scenario.Sliwa.type] = correlators(Scenario.Sliwa,
    Vec[Rational](4, 0, 0, 0, -1, -1, 0, -1, -1, 0, -1, 1, -1, -1, 0, 1, 0, 1, 0, -1, 1, 1, 0, -1, -1, 1, 0))

  val Sliwa10Sym: Expr[Scenario.Sliwa.type] = correlators(Scenario.Sliwa,
    Vec[Rational](4, 0, 0, 0, -1, -1, 0, 1, 1, 0, -1, 1, -1, -1, 0, -1, 0, -1, 0, -1, 1, 1, 0, -1, 1, -1, 0))

  val Sliwa7: Expr[Scenario.Sliwa.type] = correlators(Scenario.Sliwa,
    Vec[Rational](4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -3, -1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1))

  val Sliwa4: Expr[Scenario.Sliwa.type] = correlators(Scenario.Sliwa,
    Vec[Rational](2, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1, 0))

  val I3322: Expr[Scenario.I3322.type] = collinsGisin(Scenario.I3322,
    Vec[Rational](0, -1, 0, 0, -2, 1, 1, 1, -1, 1, 1, -1, 0, 1, -1, 0))

  val canonicalPositivity: Expr[Scenario._112.type] = Expr(Scenario._112, Vec[Rational](-1,1))

  val canonicalCHSH: Expr[Scenario.CHSH.type] = Expr(Scenario.CHSH, Vec[Rational](-1, 1, -1, 1, 1, -1, 1, -1, -1, 1, 1, -1, 1, -1, -1, 1))

}
