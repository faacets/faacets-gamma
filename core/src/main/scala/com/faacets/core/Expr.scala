package com.faacets
package core

import spire.math.{Rational, SafeLong}
import spire.algebra.VectorSpace

import com.faacets.core.repr.ReverseKronHelpers.revKronMatVec
import scalin.immutable.dense._
import scalin.immutable.{Mat, Vec}
import scala.reflect.classTag

import cats.data.{Validated, ValidatedNel}

import spire.syntax.cfor._

import scalin.syntax.all._

import net.alasc.bsgs.FixingPartition
import net.alasc.domains.Partition
import net.alasc.finite.Grp

trait GenExpr extends PVec { expr =>

  /** Computes the inner product between this expression and the given behavior. */
  def inner(corr: Behavior.Aux[S]): Rational = expr.coefficients.dot(corr.coefficients)

}

trait DExpr extends GenExpr {

  def prefix = "DExpr"

  def classTagV = classTag[DExpr]

  type V = DExpr

  /** Decomposes this expression in the nonsignaling and its biorthogonal subspace.
    * A `Expr` in a signaling representation can be projected into the non-signaling subspace.
    * The method `toNonSignaling` can be used to that effect: it returns the non-signaling
    * projected `Expr` in a non-signaling representation, and a possibly signaling `Expr`
    * containing only the signaling and proper normalization terms.
    */
  def split: (Expr.Aux[S], DExpr.Aux[S]) = {
    val nsCoeffs = DExpr.changeBasis(scenario, p => p.matrices.matProjectionInSP, coefficients)
    val sCoeffs = coefficients - nsCoeffs
    val nsExpr = Expr.applyUnsafe(scenario: S, nsCoeffs)
    val sDExpr = DExpr.apply(scenario: S, sCoeffs)
    (nsExpr, sDExpr)
  }

  /** Projection in the nonsignaling subspace, commuting with relabelings. */
  def projected: Expr.Aux[S] = {
    val nsCoeffs = DExpr.changeBasis(scenario, p => p.matrices.matProjectionInSP, coefficients)
    Expr.applyUnsafe(scenario: S, nsCoeffs)
  }

}

object DExpr {

  def changeBasis(scenario: Scenario, matChoice: Party => Mat[Rational], coefficients: Vec[Rational]): Vec[Rational] =
    revKronMatVec(scenario.parties.map(p => matChoice(p).t), coefficients)

  implicit def vectorSpace[S <: Scenario with Singleton](implicit witness: shapeless.Witness.Aux[S]): VectorSpace[DExpr.Aux[S], Rational] =
    new VectorSpace[DExpr.Aux[S], Rational] {

      def scenario: S = witness.value

      def scalar = spire.math.Rational.RationalAlgebra

      def timesl(r: Rational, v: DExpr.Aux[S]): DExpr.Aux[S] =
        if (r.isZero) zero
        else DExpr(scenario, v.coefficients * r)

      def negate(x: DExpr.Aux[S]): DExpr.Aux[S] = apply(scenario, -x.coefficients)

      def zero: DExpr.Aux[S] = DExpr(scenario, Vec.fillConstant(scenario.shapeP.size)(Rational.zero))

      def plus(x: DExpr.Aux[S], y: DExpr.Aux[S]): DExpr.Aux[S] = DExpr(scenario, x.coefficients + y.coefficients)

      override def minus(x: DExpr.Aux[S], y: DExpr.Aux[S]): DExpr.Aux[S] = DExpr(scenario, x.coefficients - y.coefficients)

    }

  def properNormalizationTest(scenario: Scenario): DExpr.Aux[scenario.type] = {
    val ratio = Rational(SafeLong.one, scenario.nInputTuples)
    val pCoefficients = Vec.fillConstant(scenario.shapeP.size)(ratio)
    apply(scenario, pCoefficients)
  }

  def normalizedSubspaceTests(scenario: Scenario): Iterable[DExpr.Aux[scenario.type]] = {
    val sub = new Array[Int](scenario.nParties)
    val buffer = collection.mutable.ArrayBuffer.newBuilder[DExpr.Aux[scenario.type]]
    cforRange(1 until scenario.shapeSC.size) { ind =>
      scenario.shapeSC.ind2sub(ind, sub)
      if (scenario.isCNormalizationIndex(sub)) {
        val scCoeffs = Vec.fromMutable[Rational](scenario.shapeSC.size, 0) { vec => vec(ind) := Rational.one }
        buffer += DExpr.fullCorrelators(scenario, scCoeffs)
      }
    }
    buffer.result()
  }

  def nonSignalingSubspaceTests(scenario: Scenario): Iterable[DExpr.Aux[scenario.type]] = {
    val sub = new Array[Int](scenario.nParties)
    val buffer = collection.mutable.ArrayBuffer.newBuilder[DExpr.Aux[scenario.type]]
    cforRange(1 until scenario.shapeSC.size) { ind =>
      scenario.shapeSC.ind2sub(ind, sub)
      if (scenario.isCSignalingIndex(sub)) {
        val scCoeffs = Vec.fromMutable[Rational](scenario.shapeSC.size, 0) { vec => vec(ind) := Rational.one }
        buffer += DExpr.fullCorrelators(scenario, scCoeffs)
      }
    }
    buffer.result()
  }

  def fullCollinsGisin(scenario: Scenario, collinsGisinCoefficients: Vec[Rational]): DExpr.Aux[scenario.type] = {
    val pCoefficients = changeBasis(scenario, _.matrices.matSGfromSP, collinsGisinCoefficients)
    apply(scenario, pCoefficients)
  }

  def fullCorrelators(scenario: Scenario, correlatorsCoefficients: Vec[Rational]): DExpr.Aux[scenario.type] = {
    val pCoefficients = changeBasis(scenario, _.matrices.matSCfromSP, correlatorsCoefficients)
    apply(scenario, pCoefficients)
  }

  def validateInScenario[S <: Scenario with Singleton](scenario: S): Vec[Rational] => ValidatedNel[String, DExpr.Aux[S]] = {
    coefficients =>
      val correctLength = scenario.shapeP.size
      val coeffLength = coefficients.length
      if (coeffLength != correctLength) Validated.invalidNel(s"Invalid coefficients length, is $coeffLength, should be $correctLength")
      else Validated.Valid(apply(scenario: S, coefficients))
  }

  def apply(scenario0: Scenario, coefficients0: Vec[Rational]): DExpr.Aux[scenario0.type] = {
    require(coefficients0.length == scenario0.shapeP.size)
    new DExpr {
      type S = scenario0.type
      val scenario: S = scenario0
      val coefficients = coefficients0
    }
  }

  type Aux[S0 <: Scenario with Singleton] = DExpr { type S = S0 }

}

/** Describes a Bell expression. */
trait Expr extends NDVec with GenExpr {

  def prefix = "Expr"

  type V = Expr

  def classTagV = classTag[Expr]

  def toDExpr: DExpr.Aux[S] = DExpr(scenario: S, coefficients)

}

object Expr extends NDVecBuilder[Expr, NDVecBuilder.ExprAux] {

  import DExpr.changeBasis

  type Aux[S0 <: Scenario with Singleton] = Expr { type S = S0 }

  implicit def vectorSpace[S <: Scenario with Singleton](implicit witness: shapeless.Witness.Aux[S]): VectorSpace[Expr.Aux[S], Rational] =
    new VectorSpace[Expr.Aux[S], Rational] {

    import NDVec.attributes.symmetryGroup

    def scenario: S = witness.value

    def scalar = spire.math.Rational.RationalAlgebra

    def timesl(r: Rational, v: Expr.Aux[S]): Expr.Aux[S] =
      if (r.isZero) zero else {
        val res = applyUnsafe(scenario, v.coefficients * r)
        symmetryGroup.get(v)(symmetryGroup.forNDVec) match {
          case Some(grp) => symmetryGroup(res)(grp)
          case None =>
        }
        res
      }

    def negate(x: Expr.Aux[S]): Expr.Aux[S] = {
      val res = applyUnsafe(scenario, -x.coefficients)
      symmetryGroup.get(x) match {
        case Some(grp) => symmetryGroup(res)(grp)
        case None =>
      }
      res
    }

    def zero: Expr.Aux[S] = {
      val res = applyUnsafe(scenario, Vec.fillConstant(scenario.shapeP.size)(Rational.zero))
      symmetryGroup(res)(scenario.group)
      res
    }

    def plus(x: Expr.Aux[S], y: Expr.Aux[S]): Expr.Aux[S] = applyUnsafe(scenario, x.coefficients + y.coefficients)

    override def minus(x: Expr.Aux[S], y: Expr.Aux[S]): Expr.Aux[S] = applyUnsafe(scenario, x.coefficients - y.coefficients)

  }


  def inNonSignalingSubspace(scenario: Scenario, coefficients: Vec[Rational]): Boolean = {
    val pCoefficients = changeBasis(scenario, p => p.matrices.matProjectionInSP, coefficients)
    coefficients == pCoefficients // TODO: Eq[Vec[Rational]]
  }

  def apply(scenario0: Scenario, coefficients0: Vec[Rational]): Expr.Aux[scenario0.type] = {
    require(inNonSignalingSubspace(scenario0, coefficients0))
    applyUnsafe(scenario0, coefficients0)
  }

  def applyUnsafe(scenario0: Scenario, coefficients0: Vec[Rational]): Expr.Aux[scenario0.type] = {
    require(coefficients0.length == scenario0.shapeP.size)
    new Expr {
      type S = scenario0.type
      val scenario: S = scenario0
      val coefficients = coefficients0
    }
  }

  def collinsGisin(scenario: Scenario, collinsGisinCoefficients: Vec[Rational]) = {
    val pCoefficients = changeBasis(scenario, p => p.matrices.matNGfromSG * p.matrices.matSGfromSP, collinsGisinCoefficients)
    applyUnsafe(scenario, pCoefficients)
  }

  def correlators(scenario: Scenario, correlatorsCoefficients: Vec[Rational]) = {
    val pCoefficients = changeBasis(scenario, p => p.matrices.matNCfromSC * p.matrices.matSCfromSP, correlatorsCoefficients)
    applyUnsafe(scenario, pCoefficients)
  }

  def averageNormalization(scenario: Scenario): Expr.Aux[scenario.type] = {
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
