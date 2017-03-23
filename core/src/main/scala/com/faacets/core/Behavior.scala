package com.faacets
package core

import scala.reflect.classTag
import scala.util.{Failure, Success, Try}

import spire.math.{Rational, SafeLong}
import spire.syntax.cfor._
import spire.util.Opt

import com.faacets.core.repr.ReverseKronHelpers.revKronMatVec
import scalin.immutable.dense._

import net.alasc.algebra._
import scalin.immutable.Vec
import scalin.syntax.all._

/** Describes a behavior in a causal scenario. */
trait Behavior extends NDVec {

  type V = Behavior

  def classTagV = classTag[Behavior]


  override def toString = s"Behavior($scenario, $coefficients)"

  /*
  def builder = Corr

  def normalization: Rational = coefficients.sum / scenario.nInputTuples

  def toNonSignalingSubspace: (Corr[S], Corr[S]) = {
    val sub = new Array[Int](scenario.nParties)
    val scCoeffs = SCRepresentation.fromCorr(this).coefficients
    val nsCoeffs = tabulate(scCoeffs.length) { ind =>
      scenario.shapeSC.ind2sub(ind, sub)
      if (scenario.isCSignalingIndex(sub)) Rational.zero else scCoeffs(ind)
    }
    val sCoeffs = scCoeffs - nsCoeffs
    (SCRepresentation.corr(scenario: S, nsCoeffs).value, SCRepresentation.corr(scenario: S, sCoeffs).value)
  }

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


    def toNonSignaling: (Corr, Corr) = ??? // TODO implement*/

  /*
  /** Returns these correlations with the given visibility, mixed with the uniformly
    * random correlations. */
  def withVisibility(v: Rational) = {
    val corr0 = Corr.uniformlyRandom(scenario)
    val newCoefficients = (coefficients * v) + (corr0.coefficients * (1 - v))
    val res = new Corr[S](scenario, newCoefficients)
    this.attr.get(PVec.symmetryGroup) match {
      case Opt(grp) if !v.isZero => res._attrUpdate(PVec.symmetryGroup, grp)
      case _ =>
    }
    res
  }*/

}

object Behavior {

  type Aux[S0 <: Scenario with Singleton] = Behavior { type S = S0 }

  def apply(scenario0: Scenario, coefficients0: Vec[Rational]): Behavior.Aux[scenario0.type] = {
    ???
  }

  def applyUnsafe(scenario0: Scenario, coefficients0: Vec[Rational]): Behavior.Aux[scenario0.type] =
    new Behavior {
      type S = scenario0.type
      val scenario: S = scenario0
      val coefficients = coefficients0
    }

  def collinsGisin(scenario: Scenario, collinsGisinCoefficients: Vec[Rational]): Behavior.Aux[scenario.type] = {
    val pCoefficients =
      revKronMatVec(scenario.parties.map(p => p.matrices.matSPfromSG * p.matrices.matSGfromNG), collinsGisinCoefficients)
    applyUnsafe(scenario, pCoefficients)
  }

  def correlators(scenario: Scenario, correlatorsCoefficients: Vec[Rational]): Behavior.Aux[scenario.type] = {
    val pCoefficients =
      revKronMatVec(scenario.parties.map(p => p.matrices.matSPfromSC * p.matrices.matSCfromNC), correlatorsCoefficients)
    applyUnsafe(scenario, pCoefficients)
  }

  def uniformlyRandom(scenario: Scenario): Behavior.Aux[scenario.type] = {
    val coefficients = scenario.tabulateP { (aInd, xInd) =>
      var nOutputs: SafeLong = SafeLong.one
      cforRange(0 until xInd.length) { p =>
        val x = xInd(p)
        val nOutputsForInput = scenario.parties(p).inputs(x)
        nOutputs *= nOutputsForInput
      }
      Rational(1, nOutputs)
    }
    applyUnsafe(scenario, coefficients)
  }

  def prBox = correlators(Scenario.CHSH, Vec[Rational](1, 0, 0, 0, 1, 1, 0, 1, -1))

}
