package com.faacets
package core

import scala.reflect.ClassTag
import spire.algebra._
import spire.syntax.order._
import spire.syntax.partialAction._
import spire.syntax.cfor._
import spire.math.Rational
import cats.kernel.Comparison
import com.faacets.consolidate.syntax.all._
import net.alasc.perms.default._
import scalin.immutable.Vec
import net.alasc.attributes.Attributable
import net.alasc.finite.Grp
import com.faacets.data.instances.vec._
import com.faacets.data.instances.grp._
import com.faacets.consolidate.instances.all._
import com.faacets.core.perm.ShapeLattice
import com.faacets.data.instances.textable._
import spire.algebra.partial.PartialAction

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
abstract class PVec[V <: PVec[V]] extends Attributable { lhs: V =>

  def prefix: String

  override def toString = s"$prefix($scenario, $coefficients)"

  val scenario: Scenario

  implicit def classTagV: ClassTag[V]

  def coefficients: Vec[Rational]

  override def hashCode: Int = scenario.hashCode + coefficients.hashCode

  override def equals(any: Any) = classTagV.unapply(any) match {
    case Some(that) => this === that
    case None => false
  }

  def ===(rhs: PVec[V]): Boolean = ((lhs.scenario: Scenario) === (rhs.scenario: Scenario)) && (lhs.coefficients == rhs.coefficients) // TODO use Eq

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
  def <|+|(r: Relabeling): V = {

    val rLattice = ShapeLattice(r)

    if (!(rLattice <= scenario.shapeLattice)) throw new IllegalArgumentException(s"Relabeling $r cannot be applied in scenario $scenario")
    else {
      import scalin.immutable.dense._
      implicit def action: PartialAction[Vec[Rational], Relabeling] = net.alasc.std.vec.vecPermutation[Rational, Vec[Rational], Relabeling](scenario.probabilityAction, implicitly, implicitly)
      builder.updatedWithSymmetryGroup(lhs, scenario, (coefficients <|+|? r).get, g => Some(g.conjugatedBy(r)))
    }

  }

}

trait PVecEq[V <: PVec[V]] extends Eq[V] {

  def eqv(lhs: V, rhs: V): Boolean = (lhs.scenario === rhs.scenario) && (lhs.coefficients == rhs.coefficients) // TODO Eq[Vec[Rational]]

}



trait PVecBuilder[V <: PVec[V]] {

  def apply(scenario: Scenario, coefficients: Vec[Rational]): V

  /** Returns a new Bell vector using the provided scenario and coefficients, with possible symmetry group update
    *
    * @param original        Original Bell vector (used when the symmetry group can be updated)
    * @param newScenario     Scenario of the updated Bell vector
    * @param newCoefficients Coefficients of the updated Bell vector
    * @param symGroupF       Function that optionally provides the updated symmetry group, when it has
    *                        already been computed for `original`
    */
  protected[faacets] def updatedWithSymmetryGroup(original: V, newScenario: Scenario, newCoefficients: Vec[Rational],
                                                  symGroupF: Grp[Relabeling] => Option[Grp[Relabeling]]): V

  implicit val lexicographicOrder: LexicographicOrder[V] = new LexicographicOrder[V] {

    def partialComparison(xe: V, ye: V): Option[Comparison] = {
      if (xe.scenario =!= ye.scenario) return None
      val x = xe.coefficients
      val y = ye.coefficients
      cforRange(0 until x.length) { i =>
        val c = Order[Rational].comparison(x(i), y(i))
        if (c != Comparison.EqualTo) return Some(c)
      }
      Some(Comparison.EqualTo)
    }

  }

}
