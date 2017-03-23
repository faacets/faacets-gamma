package com.faacets
package core

/*
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

import net.alasc.finite.Rep.algebra._
import net.alasc.algebra._
import net.alasc.finite.Rep.convert._
import polyta._

import net.alasc.attributes.{Attr, Attributable}
import net.alasc.domains.Partition
import net.alasc.finite.Grp

/** Base class for Bell expressions and correlations.
  *
  * A `BellVec` is defined by:
  *
  * - a `scenario`,
  * - coefficients `coefficients` of a vector in the space or the dual space.
  *
  * Objects in a `Scenario` such as correlations are described by `Corr` objects.
  * Linear functionals on correlations and decompositions are the dual of these and are called `Expr`.
  *
  * `Corr` and `Expr` both derive from `BellVec` which describes a generic object
  * either in the space of probability distributions or its dual.
  */
abstract class BellVec[V[X <: Scenario with Singleton] <: BellVec[V, X], S <: Scenario with Singleton] extends Attributable {
  lhs: V[S] =>

  implicit def classTagV: ClassTag[V[S]]

  override def hashCode: Int = scenario.hashCode + coefficients.hashCode

  override def equals(any: Any) = classTagV.unapply(any) match {
    case Some(that) => this === that
    case None => false
  }

  val scenario: S

  def coefficients: Vec[Rational]

  implicit def VecEq[A:Eq]: Eq[Vec[A]] = new Eq[Vec[A]] {

    def eqv(lhs: Vec[A], rhs: Vec[A]): Boolean = (lhs.length == rhs.length) && {
      cforRange(0 until lhs.length) { k =>
        if (lhs(k) =!= rhs(k)) return false
      }
      true
    }

  }

  def ===(rhs: V[S]): Boolean = lhs.coefficients === rhs.coefficients

  def coefficient(aArray: Array[Int], xArray: Array[Int]): Rational = {
    var ind = 0
    cforRange(0 until scenario.nParties) { p =>
      val partySub = scenario.parties(p).shapeP.offsets(xArray(p)) + aArray(p)
      ind = ind + scenario.shapeP.factors(p) * partySub
    }
    coefficients(ind)
  }

  def isInNonSignalingSubspace: Boolean

  def toNonSignalingSubspace: (V[S], V[S])


}

object BellVec {

  val symmetryGroup = Attr[Grp[Relabeling]]("symmetryGroup")

  implicit val exprSymmetryGroupCompute: BellVec.symmetryGroup.Compute[Expr[_ <: Scenario with Singleton]] =
    BellVec.symmetryGroup.computeFor[Expr[_ <: Scenario with Singleton]] { bv =>
      val partition = Partition.fromSeq(bv.coefficients.toIndexedSeq)
      bv.scenario.probabilityGroup.fixingPartition(partition)
    }

  implicit val corrSymmetryGroupCompute: BellVec.symmetryGroup.Compute[Corr[_ <: Scenario with Singleton]] =
    BellVec.symmetryGroup.computeFor[Corr[_ <: Scenario with Singleton]] { bv =>
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

}


/*
abstract class Vec[V <: Vec[V]: ClassTag] {

  def constant: Rational

  def terms: IndexedSeq[Term[Element]] = repr.Term.all(scenario, representation)

  def prettyExpression: pretty.Expression[Term[Element], Rational] =
    pretty.Expression((terms.toSeq zip coefficients.toIndexedSeq).filterNot(_._2 === 0))
}
*/
*/