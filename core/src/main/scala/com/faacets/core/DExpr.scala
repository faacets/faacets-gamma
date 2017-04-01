package com.faacets.core

import cats.data.{Validated, ValidatedNel}
import com.faacets.core.repr.ReverseKronHelpers.revKronMatVec
import com.faacets.core.text.{FullTerm, Term, TermType}
import scalin.immutable.{Mat, Vec}
import spire.algebra.VectorSpace
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.eq._
import scalin.immutable.dense._
import scalin.syntax.all._
import cats.syntax.traverse._
import cats.instances.vector._
import net.alasc.finite.Grp

import scala.reflect.classTag

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

  def unary_- : DExpr = new DExpr(scenario, -coefficients)

  def +(rhs: DExpr): DExpr = {
    require(lhs.scenario === rhs.scenario)
    new DExpr(scenario, lhs.coefficients + rhs.coefficients)
  }

  def *:(r: Rational): DExpr =
    if (r.isZero) DExpr.zero(scenario)
    else new DExpr(scenario, coefficients * r)

  def fullExpression: String = {
    val coeffTerms = (0 until coefficients.length).filterNot(i => coefficients(i).isZero).map { ind =>
      val (aa, xx) = scenario.ind2subP(ind)
      (coefficients(ind), FullTerm(aa, xx))
    }
    Term.printExpression(coeffTerms)
  }

  def fullTable: Table =
    if (scenario.nParties == 1) Table(coefficients.toRowMat)
    else if (scenario.nParties == 2) Table(coefficients.reshape(scenario.parties(0).shapeP.size, scenario.parties(1).shapeP.size).t)
    else sys.error("Scenarios with > 2 parties are not supported")

}

object DExpr extends PVecBuilder[DExpr] {

  protected[faacets] def updatedWithSymmetryGroup(original: DExpr, newScenario: Scenario, newCoefficients: Vec[Rational],
                                                  symGroupF: (Grp[Relabeling]) => Option[Grp[Relabeling]]): DExpr = apply(newScenario, newCoefficients)


  def parseExpression(scenario: Scenario, expression: String): ValidatedNel[String, DExpr] = {
    Term.parseExpression(expression).andThen { coeffTerms =>
      val termTypes: Set[TermType] = coeffTerms.map(_._3.termType).toSet - TermType.constant
      if (termTypes.size > 1)
        Validated.invalidNel("Mixes several expression types: " + termTypes.map(_.name).mkString(", "))
      else
        coeffTerms.map {
          case (coeff, termString, term) => term.validate(scenario).map( dExpr => coeff *: dExpr ).leftMap(_.map(s"Term '${termString}' : " + _))
        }.sequenceU.map(_.fold(DExpr.zero(scenario))(_+_))
    }
  }

  def changeBasis(scenario: Scenario, matChoice: Party => Mat[Rational], coefficients: Vec[Rational]): Vec[Rational] =
    revKronMatVec(scenario.parties.map(p => matChoice(p).t), coefficients)

  def zero(scenario: Scenario): DExpr = DExpr(scenario, Vec.fillConstant(scenario.shapeP.size)(Rational.zero))

  def vectorSpaceForScenario(scenario: Scenario): VectorSpace[DExpr, Rational] = new VectorSpace[DExpr, Rational] {

    def scalar = spire.math.Rational.RationalAlgebra

    def timesl(r: Rational, v: DExpr): DExpr = {
      require(v.scenario === scenario)
      r *: v
    }

    def negate(x: DExpr): DExpr = {
      require(x.scenario === scenario)
      -x
    }

    def zero: DExpr = DExpr.zero(scenario)

    def plus(x: DExpr, y: DExpr): DExpr = {
      require(x.scenario === scenario)
      require(y.scenario === scenario)
      x + y
    }

  }

  def properNormalizationTest(scenario: Scenario): DExpr = Expr.constant(scenario).toDExpr

  def normalizedSubspaceTests(scenario: Scenario): Iterable[DExpr] = {
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

  def nonSignalingSubspaceTests(scenario: Scenario): Iterable[DExpr] = {
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

}