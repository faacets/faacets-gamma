package com.faacets
package core

import scala.reflect.classTag

import spire.math.{Rational, SafeLong}
import spire.syntax.cfor._

import com.faacets.core.repr.ReverseKronHelpers.revKronMatVec
import scalin.immutable.dense._

import scalin.immutable.{Vec, Mat}

/** Describes a behavior in a causal scenario. */
class Behavior protected (val scenario: Scenario, val coefficients: Vec[Rational]) extends NDVec[Behavior] {

  def builder = Behavior

  def prefix = "Behavior"

  def classTagV = classTag[Behavior]

  override def toString = s"Behavior($scenario, $coefficients)"

  /** Returns these correlations with the given visibility, mixed with the uniformly
    * random correlations. */
  def withVisibility(v: Rational): Behavior = {
    val corr0 = Behavior.uniformlyRandom(scenario)
    val newCoefficients = (coefficients * v) + (corr0.coefficients * (1 - v))
    val res = Behavior(scenario, newCoefficients)
    NDVec.attributes.symmetryGroup.get(this) match {
      case Some(grp) if !v.isZero =>
        NDVec.attributes.symmetryGroup(res) { grp }
      case None =>
    }
    res
  }

  protected def inBasis(matChoice: Party => Mat[Rational]): Vec[Rational] =
    revKronMatVec(scenario.parties.map(p => matChoice(p)), coefficients)

  def correlators: Vec[Rational] = inBasis(p => p.matrices.matNCfromSC * p.matrices.matSCfromSP)

  def collinsGisin: Vec[Rational] = inBasis(p => p.matrices.matNGfromSG * p.matrices.matSGfromSP)

}

object Behavior extends NDVecBuilder[Behavior] {

  implicit def builder: NDVecBuilder[Behavior] = this

  def inNonSignalingSubspace(scenario: Scenario, coefficients: Vec[Rational]): Boolean = {
    val pCoefficients = changeBasis(scenario,
      p => p.matrices.matSPfromSG * p.matrices.matSGfromNG * p.matrices.matNGfromSG * p.matrices.matSGfromSP,
      coefficients)
    coefficients == pCoefficients // TODO: Vec[Rational] should use Eq
  }

  def changeBasis(scenario: Scenario, matChoice: Party => Mat[Rational], coefficients: Vec[Rational]): Vec[Rational] =
    revKronMatVec(scenario.parties.map(matChoice), coefficients)

  def apply(scenario: Scenario, coefficients: Vec[Rational]): Behavior = {
    require(inNonSignalingSubspace(scenario, coefficients))
    new Behavior(scenario, coefficients)
  }

  def applyUnsafe(scenario: Scenario, coefficients: Vec[Rational]): Behavior = new Behavior(scenario, coefficients)

  def collinsGisin(scenario: Scenario, collinsGisinCoefficients: Vec[Rational]): Behavior = {
    val pCoefficients = changeBasis(scenario, p => p.matrices.matSPfromSG * p.matrices.matSGfromNG, collinsGisinCoefficients)
    applyUnsafe(scenario, pCoefficients)
  }

  def correlators(scenario: Scenario, correlatorsCoefficients: Vec[Rational]): Behavior = {
    val pCoefficients = changeBasis(scenario, p => p.matrices.matSPfromSC * p.matrices.matSCfromNC, correlatorsCoefficients)
    applyUnsafe(scenario, pCoefficients)
  }

  def uniformlyRandom(scenario: Scenario): Behavior = {
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
