package com.faacets
package core

import scala.reflect.classTag

import spire.math.{Rational, SafeLong}
import spire.syntax.cfor._

import com.faacets.core.repr.ReverseKronHelpers.revKronMatVec
import scalin.immutable.dense._

import scalin.immutable.{Vec, Mat}

/** Describes a behavior in a causal scenario. */
trait Behavior extends NDVec {

  def prefix = "Behavior"

  type V = Behavior

  def classTagV = classTag[Behavior]

  override def toString = s"Behavior($scenario, $coefficients)"

  /** Returns these correlations with the given visibility, mixed with the uniformly
    * random correlations. */
  def withVisibility(v: Rational): Behavior.Aux[S] = {
    val corr0 = Behavior.uniformlyRandom(scenario)
    val newCoefficients = (coefficients * v) + (corr0.coefficients * (1 - v))
    val res = Behavior(scenario: S, newCoefficients)
    NDVec.attributes.symmetryGroup.get(this) match {
      case Some(grp) if !v.isZero =>
        NDVec.attributes.symmetryGroup(res) { grp }
      case None =>
    }
    res
  }

}

object Behavior {

  type Aux[S0 <: Scenario with Singleton] = Behavior { type S = S0 }

  def changeBasis(scenario: Scenario, matChoice: Party => Mat[Rational], coefficients: Vec[Rational]): Vec[Rational] =
    revKronMatVec(scenario.parties.map(matChoice), coefficients)

  def apply(scenario0: Scenario, coefficients0: Vec[Rational]): Behavior.Aux[scenario0.type] = {
    val pCoefficients = changeBasis(scenario0,
      p => p.matrices.matSPfromSG * p.matrices.matSGfromNG * p.matrices.matNGfromSG * p.matrices.matSGfromSP,
      coefficients0)
    require(coefficients0 == pCoefficients) // TODO: Vec[Rational] should use Eq
    applyUnsafe(scenario0, coefficients0)
  }

  def applyUnsafe(scenario0: Scenario, coefficients0: Vec[Rational]): Behavior.Aux[scenario0.type] =
    new Behavior {
      type S = scenario0.type
      val scenario: S = scenario0
      val coefficients = coefficients0
    }

  def collinsGisin(scenario: Scenario, collinsGisinCoefficients: Vec[Rational]): Behavior.Aux[scenario.type] = {
    val pCoefficients = changeBasis(scenario, p => p.matrices.matSPfromSG * p.matrices.matSGfromNG, collinsGisinCoefficients)
    applyUnsafe(scenario, pCoefficients)
  }

  def correlators(scenario: Scenario, correlatorsCoefficients: Vec[Rational]): Behavior.Aux[scenario.type] = {
    val pCoefficients = changeBasis(scenario, p => p.matrices.matSPfromSC * p.matrices.matSCfromNC, correlatorsCoefficients)
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
