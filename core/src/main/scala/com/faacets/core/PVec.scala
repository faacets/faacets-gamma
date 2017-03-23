package com.faacets
package core

import scala.util.{Failure, Success, Try}
import scala.reflect.{ClassTag, classTag}

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.innerProductSpace._
import spire.syntax.monoid._
import spire.syntax.cfor._
import spire.math.Rational

import net.alasc.perms.default._
import com.faacets.core.perm.Relabeling
import scalin.immutable.dense._
import scalin.immutable.Vec

import net.alasc.algebra._
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
/*
  def isInNonSignalingSubspace: Boolean

  def toNonSignalingSubspace: (V[S], V[S])

*/

object PVec {
  /*


  implicit val exprSymmetryGroupCompute: PVec.symmetryGroup.Compute[Expr[_ <: Scenario with Singleton]] =
    PVec.symmetryGroup.computeFor[Expr[_ <: Scenario with Singleton]] { bv =>
      val partition = Partition.fromSeq(bv.coefficients.toIndexedSeq)
      bv.scenario.probabilityGroup.fixingPartition(partition)
    }

  implicit val corrSymmetryGroupCompute: PVec.symmetryGroup.Compute[Corr[_ <: Scenario with Singleton]] =
    PVec.symmetryGroup.computeFor[Corr[_ <: Scenario with Singleton]] { bv =>
      val partition = Partition.fromSeq(bv.coefficients.toIndexedSeq)
      bv.scenario.probabilityGroup.fixingPartition(partition)
    }

  /** Computes the inner product between Expr and Corr.
    *
    * The product of betwen signaling and non-signaling objects is defined by computing
    * the product in the non-signaling subspace.
    */
  def inner[S <: Scenario with Singleton](expr: Expr[S], corr: Corr[S]): Rational =
    expr.coefficients.dot(corr.coefficients)
*/
}


/*
  implicit def VecEq[A:Eq]: Eq[Vec[A]] = new Eq[Vec[A]] {

    def eqv(lhs: Vec[A], rhs: Vec[A]): Boolean = (lhs.length == rhs.length) && {
      cforRange(0 until lhs.length) { k =>
        if (lhs(k) =!= rhs(k)) return false
      }
      true
    }

  }

abstract class Vec[V <: Vec[V]: ClassTag] {

  def constant: Rational

  def terms: IndexedSeq[Term[Element]] = repr.Term.all(scenario, representation)

  def prettyExpression: pretty.Expression[Term[Element], Rational] =
    pretty.Expression((terms.toSeq zip coefficients.toIndexedSeq).filterNot(_._2 === 0))
}
*/
