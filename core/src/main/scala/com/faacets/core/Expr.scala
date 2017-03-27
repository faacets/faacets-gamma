package com.faacets
package core

import spire.math.Rational

import spire.algebra.VectorSpace

import com.faacets.core.repr.ReverseKronHelpers.revKronMatVec

import scalin.immutable.dense._
import scalin.immutable.{Mat, Vec}

import scala.reflect.classTag

import com.faacets.core.DExpr.Aux
import scalin.syntax.all._

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
/*

def isInNonSignalingSubspace: Boolean = {
  val sub = new Array[Int](scenario.nParties)
  val scCoeffs = repr.SCRepresentation.fromCorr(this).coefficients
  cforRange(0 until scCoeffs.length) { ind =>
    scenario.shapeSC.ind2sub(ind, sub)
    if (scenario.isCSignalingIndex(sub) && !scCoeffs(ind).isZero)
      return false
  }
  true
}

def isProperlyNormalized: Boolean = {
  val sub = new Array[Int](scenario.nParties)
  val scCoeffs = repr.SCRepresentation.fromCorr(this).coefficients
  if (!scCoeffs(0).isOne) return false
  cforRange(1 until scCoeffs.length) { ind =>
    scenario.shapeSC.ind2sub(ind, sub)
    if (scenario.isCProperNormalizationIndex(sub) && !scCoeffs(ind).isZero)
      return false
  }
  true
}
*/

  /*

    override def as(toRepresentation: Representation): Try[Corr] = {
      super.as(toRepresentation).map { newCorr =>
        if (representation.isStrategy && toRepresentation.isCorrelation)
          // when converting from strategies to correlations, symmetries cannot be restored
          builder(newCorr.scenario, newCorr.representation, newCorr.coefficients, None)
        else
          newCorr
      }
    }

    /** Tests that the correlations have no proper normalization terms, and that
      * the corresponding probability distribution is normalized to 1.
      */
    def isNormalized: Boolean = (constant == 1 && !hasProperNormalizationTerms)

    /** Tests if the correlations are signaling. */
    def isSignaling: Boolean = hasSignalingTerms

*/


  def fullCollinsGisin(scenario0: Scenario, collinsGisinCoefficients: Vec[Rational]) = ???

  def fullCorrelators(scenario0: Scenario, correlatorsCoefficients: Vec[Rational]) = ???

  def apply(scenario0: Scenario, coefficients0: Vec[Rational]): DExpr.Aux[scenario0.type] =
  new DExpr {
    type S = scenario0.type
    val scenario: S = scenario0
    val coefficients = coefficients0
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

object Expr {

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

  def apply(scenario0: Scenario, coefficients0: Vec[Rational]): Expr.Aux[scenario0.type] = {
    val pCoefficients = changeBasis(scenario0, p => p.matrices.matProjectionInSP, coefficients0)
    require(coefficients0 == pCoefficients) // TODO: Eq[Vec[Rational]]
    applyUnsafe(scenario0, coefficients0)
  }

  def applyUnsafe(scenario0: Scenario, coefficients0: Vec[Rational]): Expr.Aux[scenario0.type] =
  new Expr {
    type S = scenario0.type
    val scenario: S = scenario0
    val coefficients = coefficients0
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
