package com.faacets.core

import scala.reflect.classTag

import cats.data.{Validated, ValidatedNel}
import cats.instances.vector._
import cats.syntax.traverse._
import spire.math.{Rational, SafeLong}
import spire.syntax.cfor._
import scalin.immutable.dense._
import scalin.immutable.{Mat, Vec}
import scalin.syntax.all._
import net.alasc.finite.Grp

import com.faacets.core.repr.ReverseKronHelpers.revKronMatVec
import com.faacets.core.text.{FullTerm, Term, TermType}

class DExpr protected (val scenario: Scenario, val coefficients: Vec[Rational]) extends GenExpr[DExpr] { lhs =>

  def builder = DExpr

  def inBasis(matChoice: Party => Mat[Rational]): Vec[Rational] =
    revKronMatVec(scenario.parties.map(p => matChoice(p).t), coefficients)

  def correlators: Vec[Rational] = inBasis(_.matrices.matSPfromSC)

  def collinsGisin: Vec[Rational] = inBasis(_.matrices.matSPfromSG)

  def prefix = "DExpr"

  def classTagV = classTag[DExpr]

  /** Decomposes this expression in the nonsignaling and its biorthogonal subspace.
    * A `Expr` in a signaling representation can be projected into the non-signaling subspace.
    * The method `toNonSignaling` can be used to that effect: it returns the non-signaling
    * projected `Expr` in a non-signaling representation, and a possibly signaling `Expr`
    * containing only the signaling and proper normalization terms.
    */
  def split: (Expr, DExpr) = {
    val nsCoeffs = DExpr.changeBasis(scenario, p => p.matrices.matProjectionInSP, coefficients)
    val sCoeffs = coefficients - nsCoeffs
    val nsExpr = Expr.applyUnsafe(scenario, nsCoeffs)
    val sDExpr = DExpr.apply(scenario, sCoeffs)
    (nsExpr, sDExpr)
  }

  /** Projection in the nonsignaling subspace, commuting with relabelings. */
  def projected: Expr = {
    val nsCoeffs = DExpr.changeBasis(scenario, p => p.matrices.matProjectionInSP, coefficients)
    Expr.applyUnsafe(scenario, nsCoeffs)
  }

  def expression: String = {
    val coeffTerms = (0 until coefficients.length).filterNot(i => coefficients(i).isZero).map { ind =>
      val (aa, xx) = scenario.ind2subP(ind)
      (coefficients(ind), FullTerm(aa, xx))
    }
    Term.printExpression(coeffTerms)
  }

  def fullTable_BA: Table =
    if (scenario.nParties == 1) Table(coefficients.toRowMat)
    else if (scenario.nParties == 2) Table(coefficients.reshape(scenario.parties(0).shapeP.size, scenario.parties(1).shapeP.size).t)
    else sys.error("Scenarios with > 2 parties are not supported")

}

object DExpr extends GenExprBuilder[DExpr] {

  implicit def builder: GenExprBuilder[DExpr] = this

  protected[faacets] def updatedWithSymmetryGroup(original: DExpr, newScenario: Scenario, newCoefficients: Vec[Rational],
                                                  symGroupF: (Grp[Relabeling]) => Option[Grp[Relabeling]]): DExpr =
    apply(newScenario, newCoefficients)

  def parseExpression(scenario: Scenario, expression: String): ValidatedNel[String, DExpr] = {
    Term.parseExpression(expression).andThen { coeffTerms =>
      val termTypes: Set[TermType] = coeffTerms.map(_._3.termType).toSet - TermType.constant
      if (termTypes.size > 1)
        Validated.invalidNel("Mixes several expression types: " + termTypes.map(_.name).mkString(", "))
      else
        coeffTerms.map {
          case (coeff, termString, term) =>
            term.validate(scenario).map( dExpr => coeff *: dExpr )
              .leftMap(_.map(s"Term '${termString}' : " + _))
        }.sequenceU.map(_.fold(DExpr.zero(scenario))(_+_))
    }
  }

  def changeBasis(scenario: Scenario, matChoice: Party => Mat[Rational], coefficients: Vec[Rational]): Vec[Rational] =
    revKronMatVec(scenario.parties.map(p => matChoice(p).t), coefficients)

  def zero(scenario: Scenario): DExpr = DExpr(scenario, Vec.fillConstant(scenario.shapeP.size)(Rational.zero))

  def one(scenario: Scenario): DExpr = {
    val ratio = Rational(SafeLong.one, scenario.nInputTuples)
    val pCoefficients = Vec.fillConstant(scenario.shapeP.size)(ratio)
    apply(scenario, pCoefficients)
  }

  def properNormalizationTest(scenario: Scenario): DExpr = Expr.one(scenario).toDExpr

  def normalizedSubspaceTests(scenario: Scenario): Seq[DExpr] = {
    val sub = new Array[Int](scenario.nParties)
    val buffer = collection.mutable.ArrayBuffer.newBuilder[DExpr]
    cforRange(1 until scenario.shapeSC.size) { ind =>
      scenario.shapeSC.ind2sub(ind, sub)
      if (scenario.isCNormalizationIndex(sub)) {
        val scCoeffs = Vec.fromMutable[Rational](scenario.shapeSC.size, 0) { vec => vec(ind) := Rational.one }
        buffer += DExpr.fullCorrelators(scenario, scCoeffs)
      }
    }
    buffer.result()
  }

  def nonSignalingSubspaceTests(scenario: Scenario): Seq[DExpr] = {
    val sub = new Array[Int](scenario.nParties)
    val buffer = collection.mutable.ArrayBuffer.newBuilder[DExpr]
    cforRange(1 until scenario.shapeSC.size) { ind =>
      scenario.shapeSC.ind2sub(ind, sub)
      if (scenario.isCSignalingIndex(sub)) {
        val scCoeffs = Vec.fromMutable[Rational](scenario.shapeSC.size, 0) { vec => vec(ind) := Rational.one }
        buffer += DExpr.fullCorrelators(scenario, scCoeffs)
      }
    }
    buffer.result()
  }

  def fullCollinsGisin(scenario: Scenario, collinsGisinCoefficients: Vec[Rational]): DExpr = {
    val pCoefficients = changeBasis(scenario, _.matrices.matSGfromSP, collinsGisinCoefficients)
    apply(scenario, pCoefficients)
  }

  def fullCorrelators(scenario: Scenario, correlatorsCoefficients: Vec[Rational]): DExpr = {
    val pCoefficients = changeBasis(scenario, _.matrices.matSCfromSP, correlatorsCoefficients)
    apply(scenario, pCoefficients)
  }

  def validate(scenario: Scenario, coefficients: Vec[Rational]): ValidatedNel[String, DExpr] = {
    val correctLength = scenario.shapeP.size
    val coeffLength = coefficients.length
    if (coeffLength != correctLength) Validated.invalidNel(s"Invalid coefficients length, is $coeffLength, should be $correctLength")
    else Validated.Valid(apply(scenario, coefficients))
  }

  def apply(scenario: Scenario, coefficients: Vec[Rational]): DExpr = {
    require(coefficients.length == scenario.shapeP.size, "Incorrect coefficient vector length")
    new DExpr(scenario, coefficients)
  }

  def applyUnsafe(scenario: Scenario, coefficients: Vec[Rational]): DExpr = apply(scenario, coefficients)

}