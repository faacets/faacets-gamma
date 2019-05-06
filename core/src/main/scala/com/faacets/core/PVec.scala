package com.faacets
package core

import shapeless.Witness
import spire.algebra._
import spire.algebra.partial.PartialAction
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.group._
import spire.syntax.partialAction._

import scalin.immutable.Vec

import net.alasc.attributes.Attributable
import net.alasc.finite.Grp
import net.alasc.perms.default._
import spire.util.Opt

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
abstract class PVec[V[X <: Scenario with Singleton] <: PVec[V, X], S <: Scenario with Singleton] extends Attributable { lhs: V[S] =>

  def prefix: String

  override def toString = s"$prefix($scenario, $coefficients)"

  def scenario: S

  implicit def witnessS: Witness.Aux[S] = Witness.mkWitness[S](scenario)

  def coefficients: Vec[Rational]

  override def hashCode: Int = scenario.hashCode + coefficients.hashCode

  override def equals(any: Any) = any match {
    case that: PVec[_, _] => (this.scenario == that.scenario) && (this.coefficients == that.coefficients)
    case _ => false
  }

  def coefficient(aArray: Array[Int], xArray: Array[Int]): Rational = {
    var ind = 0
    cforRange(0 until scenario.nParties) { p =>
      val partySub = scenario.parties(p).shapeP.offsets(xArray(p)) + aArray(p)
      ind = ind + scenario.shapeP.factors(p) * partySub
    }
    coefficients(ind)
  }

  def builder: PVecBuilder[V]

  /** Relabeling of a PVec.
    *
    * Relabellings/permutations can be applied to behaviors and Bell expressions. The action we describe below is the right action.
    *
    * - for the representations `NPRepr`, `SPRepr` and `WRepr` we have the following:
    *   Let \\( j = (a b ... x y ..) \\) (for correlations) the index over the coefficients \\( P(j) \\)
    *   Let \\( g \\) be a permutation in the Bell group of the `Ket` scenario.
    *   The action \\( j^g \\) is well-defined.
    *   We define the action of \\(P^g\\) such that:
    *   \\( P^g(j^g) = P(j) \\)
    *
    *   Throws if the relabeling is not compatible.
    */
  def <|+|(r: Relabeling.Aux[S]): V[S] =
    /*if (!scenario.group.contains(r))
      throw new IllegalArgumentException(s"Relabeling $r cannot be applied in scenario $scenario")
    else TODO: implement contains or make it safe */ {
      import com.faacets.data.instances.vec._
      implicit def action: PartialAction[Vec[Rational], Relabeling.Aux[S]] = vecPermutation[Rational, Relabeling.Aux[S]]((scenario: S).probabilityAction, implicitly)
      builder.updatedWithSymmetryGroup[S](lhs, (coefficients <|+|? r).get, g => Some(g.conjugatedBy(r)))
    }

}

trait PVecBuilder[V[S <: Scenario with Singleton] <: PVec[V, S]] {

  def applyUnsafe(scenario: Scenario, coefficients: Vec[Rational]): V[scenario.type]

  def apply(scenario: Scenario, coefficients: Vec[Rational]): V[scenario.type]

  /** Returns a new Bell vector using the provided scenario and coefficients, with possible symmetry group update
    *
    * @param original        Original Bell vector (used when the symmetry group can be updated)
    * @param newCoefficients Coefficients of the updated Bell vector
    * @param symGroupF       Function that optionally provides the updated symmetry group, when it has
    *                        already been computed for `original`
    */
  protected[faacets] def updatedWithSymmetryGroup[S <: Scenario with Singleton](original: V[S], newCoefficients: Vec[Rational],
                                                  symGroupF: Grp[Relabeling.Aux[S]] => Option[Grp[Relabeling.Aux[S]]]): V[S]

  implicit def lexicographicOrder[S <: Scenario with Singleton]: Order[V[S]] = new Order[V[S]] {
    override def compare(xe: V[S], ye: V[S]): Int = {
      val x = xe.coefficients
      val y = ye.coefficients
      cforRange(0 until x.length) { i =>
        val c = Order[Rational].compare(x(i), y(i))
        if (c != 0) return c
      }
      0
    }
  }

}

class VecRelabelingAction[V[X <: Scenario with Singleton] <: PVec[V, X], S <: Scenario with Singleton]
(implicit builder: PVecBuilder[V]) extends Action[V[S], Relabeling.Aux[S]] {

  def actl(r: Relabeling.Aux[S], v: V[S]): V[S] = actr(v, r.inverse)

  def actr(v: V[S], r: Relabeling.Aux[S]): V[S] = {
    implicit def witness: Witness.Aux[S] = v.witnessS
    implicit def action: PartialAction[Vec[Rational], Relabeling.Aux[S]] =
      com.faacets.data.instances.vec.vecPermutation[Rational, Relabeling.Aux[S]]((v.scenario: S).probabilityAction, implicitly)
    builder.updatedWithSymmetryGroup(v, (v.coefficients <|+|? r).get, g => Some(g.conjugatedBy(r)))
  }
}
