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


trait GenExpr extends PVec { expr =>

  /** Computes the inner product between this expression and the given behavior. */
  def inner(corr: Behavior): Rational = expr.coefficients.dot(corr.coefficients)

}

class DExpr protected (val scenario: Scenario, val coefficients: Vec[Rational]) extends GenExpr { lhs =>

  type V = DExpr

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

}

object DExpr {

  def parseExpression(scenario: Scenario, expression: String): ValidatedNel[String, DExpr] = {
    import fastparse.noApi._
    import com.faacets.data.Parsers.White._
    import com.faacets.core.text._
    (CoeffString.expr ~ End).parse(expression) match {
      case Parsed.Success(csSeq, _) =>
        val allCoeffTerms = csSeq.toVector.map {
          case CoeffString(coeff, None) => Validated.valid( (coeff, "", ConstantTerm) )
          case CoeffString(coeff, Some(termString)) =>
            (CoeffString.term ~ End).parse(termString) match {
              case Parsed.Success(term, _) => Validated.valid( (coeff, termString, term) )
              case f => Validated.invalidNel(f.toString)
            }
        }
        allCoeffTerms.sequenceU.andThen { coeffTerms =>
          val termTypes: Set[TermType] = coeffTerms.map(_._3.termType).toSet - TermType.constant
          if (termTypes.size > 1)
            Validated.invalidNel("Mixes several expression types: " + termTypes.mkString(", "))
          else
            coeffTerms.map {
              case (coeff, termString, term) => term.validate(scenario).map( dExpr => coeff *: dExpr ).leftMap(_.map(s"Term '${termString}' : " + _))
            }.sequenceU.map(_.fold(DExpr.zero(scenario))(_+_))
        }
      case f => Validated.invalidNel(f.toString)
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

}

object Expr extends NDVecBuilder[Expr] {

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
