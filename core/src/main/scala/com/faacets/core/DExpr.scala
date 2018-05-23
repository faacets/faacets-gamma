package com.faacets.core

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
import spire.algebra.Action
import spire.algebra.partial.PartialAction

/** Describes a Bell expression not necessarily in the relevant nonsignaling subspace. */
class DExpr[S <: Scenario with Singleton] protected (val scenario: S, val coefficients: Vec[Rational]) extends GenExpr[DExpr, S] { lhs =>

  def builder = DExpr

  def inBasis(matChoice: Party => Mat[Rational]): Vec[Rational] =
    revKronMatVec(scenario.parties.map(p => matChoice(p).t), coefficients)

  def correlators: Vec[Rational] = inBasis(_.matrices.matSPfromSC)

  def collinsGisin: Vec[Rational] = inBasis(_.matrices.matSPfromSG)

  def prefix = "DExpr"

  /** Decomposes this expression in the nonsignaling and its biorthogonal subspace.
    * A `Expr` in a signaling representation can be projected into the non-signaling subspace.
    * The method `toNonSignaling` can be used to that effect: it returns the non-signaling
    * projected `Expr` in a non-signaling representation, and a possibly signaling `Expr`
    * containing only the signaling and proper normalization terms.
    */
  def split: (Expr[S], DExpr[S]) = {
    val nsCoeffs = DExpr.changeBasis(scenario, p => p.matrices.matProjectionInSP, coefficients)
    val sCoeffs = coefficients - nsCoeffs
    val nsExpr = Expr.applyUnsafe(scenario: S, nsCoeffs)
    val sDExpr = DExpr.apply(scenario: S, sCoeffs)
    (nsExpr, sDExpr)
  }

  /** Projection in the nonsignaling subspace, commuting with relabelings. */
  def projected: Expr[S] = {
    val nsCoeffs = DExpr.changeBasis(scenario, p => p.matrices.matProjectionInSP, coefficients)
    Expr.applyUnsafe(scenario: S, nsCoeffs)
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

  implicit def dExprRelabelingAction[S <: Scenario with Singleton]: Action[DExpr[S], Relabeling.Aux[S]] =
    new VecRelabelingAction[DExpr, S]

  implicit def builder: GenExprBuilder[DExpr] = this

  protected[faacets] def updatedWithSymmetryGroup[S <: Scenario with Singleton](original: DExpr[S], newCoefficients: Vec[Rational],
                                                  symGroupF: (Grp[Relabeling.Aux[S]]) => Option[Grp[Relabeling.Aux[S]]]): DExpr[S] =
    apply(original.scenario: S, newCoefficients)

  def parseExpression(scenario: Scenario, expression: String): ValidatedNel[String, DExpr[scenario.type]] = {
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

  def zero(scenario: Scenario): DExpr[scenario.type] = DExpr(scenario, Vec.fillConstant(scenario.shapeP.size)(Rational.zero))

  def one(scenario: Scenario): DExpr[scenario.type] = {
    val ratio = Rational(SafeLong.one, scenario.nInputTuples)
    val pCoefficients = Vec.fillConstant(scenario.shapeP.size)(ratio)
    apply(scenario, pCoefficients)
  }

  def properNormalizationTest(scenario: Scenario): DExpr[scenario.type] = Expr.one(scenario).toDExpr

  def normalizedSubspaceTests(scenario: Scenario): Seq[DExpr[scenario.type]] = {
    val sub = new Array[Int](scenario.nParties)
    val buffer = collection.mutable.ArrayBuffer.newBuilder[DExpr[scenario.type]]
    cforRange(1 until scenario.shapeSC.size) { ind =>
      scenario.shapeSC.ind2sub(ind, sub)
      if (scenario.isCNormalizationIndex(sub)) {
        val scCoeffs = Vec.fromMutable[Rational](scenario.shapeSC.size, 0) { vec => vec(ind) := Rational.one }
        buffer += DExpr.fullCorrelators(scenario, scCoeffs)
      }
    }
    buffer.result()
  }

  def nonSignalingSubspaceTests(scenario: Scenario): Seq[DExpr[scenario.type]] = {
    val sub = new Array[Int](scenario.nParties)
    val buffer = collection.mutable.ArrayBuffer.newBuilder[DExpr[scenario.type]]
    cforRange(1 until scenario.shapeSC.size) { ind =>
      scenario.shapeSC.ind2sub(ind, sub)
      if (scenario.isCSignalingIndex(sub)) {
        val scCoeffs = Vec.fromMutable[Rational](scenario.shapeSC.size, 0) { vec => vec(ind) := Rational.one }
        buffer += DExpr.fullCorrelators(scenario, scCoeffs)
      }
    }
    buffer.result()
  }

  def fullCollinsGisin(scenario: Scenario, collinsGisinCoefficients: Vec[Rational]): DExpr[scenario.type] = {
    val pCoefficients = changeBasis(scenario, _.matrices.matSGfromSP, collinsGisinCoefficients)
    apply(scenario, pCoefficients)
  }

  def fullCorrelators(scenario: Scenario, correlatorsCoefficients: Vec[Rational]): DExpr[scenario.type] = {
    val pCoefficients = changeBasis(scenario, _.matrices.matSCfromSP, correlatorsCoefficients)
    apply(scenario, pCoefficients)
  }

  def validate(scenario: Scenario, coefficients: Vec[Rational]): ValidatedNel[String, DExpr[scenario.type]] = {
    val correctLength = scenario.shapeP.size
    val coeffLength = coefficients.length
    if (coeffLength != correctLength) Validated.invalidNel(s"Invalid coefficients length, is $coeffLength, should be $correctLength")
    else Validated.Valid(apply(scenario, coefficients))
  }

  def apply(scenario: Scenario, coefficients: Vec[Rational]): DExpr[scenario.type] = {
    require(coefficients.length == scenario.shapeP.size, "Incorrect coefficient vector length")
    new DExpr(scenario, coefficients)
  }

  def applyUnsafe(scenario: Scenario, coefficients: Vec[Rational]): DExpr[scenario.type] = apply(scenario, coefficients)

  def cglmpOriginal(scenario: Scenario): DExpr[scenario.type] = {
    val d = scenario.maxNumOutputs
    import scenario.sub2indP
    require(scenario == Scenario.nmk(2, 2, d))
    val krange = (0 to (d/2-1))
    val ineqbound = 2
    // coefficients of inequality
    val beta = Vec.fromMutable[Rational](2*2*d*d, Rational.zero) { v =>
      for (k <- krange) {
        val coeff = 1 - Rational(2 * k, d - 1)
        for (a <- 0 until d; b <- 0 until d) {
          // P(A1 - B1 = k) - P(A1 - B1 = - (k+1) )
          val ind11 = sub2indP(Array(a, b), Array(0, 0))
          val ind12 = sub2indP(Array(a, b), Array(0, 1))
          val ind21 = sub2indP(Array(a, b), Array(1, 0))
          val ind22 = sub2indP(Array(a, b), Array(1, 1))

          def cond(n: Int) = ((4 * d + n) % d == 0)

          if (cond(a - b - k))
            v(ind11) := v(ind11) + coeff
          if (cond(b - a - k - 1))
            v(ind21) := v(ind21) + coeff
          if (cond(a - b - k))
            v(ind22) := v(ind22) + coeff
          if (cond(b - a - k))
            v(ind12) := v(ind12) + coeff

          if (cond(a - b + k + 1))
            v(ind11) := v(ind11) - coeff
          if (cond(b - a + k))
            v(ind21) := v(ind21) - coeff
          if (cond(a - b + k + 1))
            v(ind22) := v(ind22) - coeff
          if (cond(b - a + k + 1))
            v(ind12) := v(ind12) - coeff
        }
      }
    }
    DExpr(scenario, beta)
  }

  def cglmpAcin(scenario: Scenario): DExpr[scenario.type] = {
    val d = scenario.maxNumOutputs
    import scenario.sub2indP
    require(scenario == Scenario.nmk(2, 2, d))
    def ind(a: Int, b: Int, x: Int, y: Int): Int = sub2indP(Array(a, b), Array(x, y))
    val beta = scalin.immutable.Vec.fromMutable[Rational](2*2*d*d, Rational.zero) { v =>
      for (a <- 0 until d; b <- 0 until d) {
        def brkt(k: Int) = ((4 * d + k) % d)
        v(ind(a, b, 0, 0)) := v(ind(a, b, 0, 0)) + brkt(a - b)
        v(ind(a, b, 1, 0)) := v(ind(a, b, 1, 0)) + brkt(b - a)
        v(ind(a, b, 1, 1)) := v(ind(a, b, 1, 1)) + brkt(a - b)
        v(ind(a, b, 0, 1)) := v(ind(a, b, 0, 1)) + brkt(b - a - 1)
      }
    }
    DExpr(scenario, beta)
  }

}
