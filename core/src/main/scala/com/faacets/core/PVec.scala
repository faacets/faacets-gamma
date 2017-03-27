package com.faacets
package core

import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import spire.math.Rational

import net.alasc.perms.default._
import scalin.immutable.Vec

import net.alasc.attributes.{Attributable, Attributes}
import net.alasc.domains.Partition
import net.alasc.finite.Grp

/** Base class for vectors in the probability space of a causal scenario.
  *
  * A `PVec` is defined by:
  *
  * - a `scenario`,
  * - coefficients `coefficients` of a vector in the space or the dual space corresponding
  *   to linear forms.
  *
  * Probability distributions in a `Scenario` are described by `Behavior` objects.
  * Linear functionals on correlations and decompositions are the dual of these and are called `Expr`.
  *
  * Expressions which are not written in the canonical basis of the causal (nonsignaling) subspace are of class `DExpr`.
  *
  */
abstract class PVec extends Attributable { lhs =>

  def prefix: String

  override def toString = s"$prefix($scenario, $coefficients)"

  type S <: Scenario with Singleton

  val scenario: S

  type V <: PVec

  implicit def classTagV: ClassTag[V]

  def coefficients: Vec[Rational]

  override def hashCode: Int = scenario.hashCode + coefficients.hashCode

  override def equals(any: Any) = classTagV.unapply(any) match {
    case Some(that) => this === that
    case None => false
  }

  def ===(rhs: PVec): Boolean = ((lhs.scenario: Scenario) === (rhs.scenario: Scenario)) && (lhs.coefficients == rhs.coefficients) // TODO use Eq

  def coefficient(aArray: Array[Int], xArray: Array[Int]): Rational = {
    var ind = 0
    cforRange(0 until scenario.nParties) { p =>
      val partySub = scenario.parties(p).shapeP.offsets(xArray(p)) + aArray(p)
      ind = ind + scenario.shapeP.factors(p) * partySub
    }
    coefficients(ind)
  }

}

trait PVecEq[V <: PVec] extends Eq[V] {

  def eqv(lhs: V, rhs: V): Boolean = ((lhs.scenario:Scenario) === (rhs.scenario:Scenario)) && (lhs.coefficients == rhs.coefficients) // TODO Eq[Vec[Rational]]

}

abstract class NDVec extends PVec {

  def symmetryGroup: Grp[Relabeling] = NDVec.attributes.symmetryGroup(this) {
    val partition = Partition.fromSeq(coefficients.toIndexedSeq)
    scenario.group.fixingPartition(scenario.probabilityAction, partition)
  }

}

object NDVec {

  object attributes extends Attributes("NDVec") {

    object symmetryGroup extends Attribute("symmetryGroup") {
      implicit def forNDVec: For[NDVec, Grp[Relabeling]] = For
    }

  }

}
