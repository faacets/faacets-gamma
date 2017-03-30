package com.faacets
package operation
package relabeling
/*
import spire.algebra.{NullboxPartialAction, Action}
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.partialOrder._
import spire.util.Nullbox

import polyta._

import net.alasc.algebra.InversePair
import net.alasc.math.Grp

import core._
import perm.{Relabeling, ShapeLattice}
import relabeling._

/** Relabeling of a Vec.
  * 
  * Relabellings/permutations can be applied to correlations, deterministic decompositions,
  * and Bell expressions. The action we describe below is the right action.
  * 
  * - for the representations `NPRepr`, `SPRepr` and `WRepr` we have the following: 
  *   Let \\( j = (a b ... x y ..) \\) (for correlations) or 
  *   \\( j = (\alpha_1 \alpha_2 ... \beta_1 \beta_2 ...) \\) (for strategy weights) be the
  *   index over the coefficients \\( P(j) \\) or \\( q(j) \\) respectively. 
  *   Let \\( g \\) be a permutation in the Bell group of the `Ket` scenario.
  *   The action \\( j^g \\) is well-defined.
  *   We define the action of \\(P^g\\) or \\( q^g \\) such that:
  *   \\( P^g(j^g) = P(j) \\) or \\(q^g(j^g) = q(j)\\).
  * - for the representation `TRepr`, we convert to `WRepr`, apply the permutation and convert back
  * - for `NGRepr` and `NCRepr`, we convert to `NPRepr`, apply the permutation and convert back
  * - for `SGRepr` and `SCRepr`, we convert to `SPRepr`, apply the permutation and convert back
  */
class VecRelabelingAction[V <: Vec[V]] extends NullboxPartialAction[V, Relabeling] {

  override def actrIsDefined(v: V, r: Relabeling): Boolean = ShapeLattice(r) <= v.scenario.shapeLattice
  override def actlIsDefined(r: Relabeling, v: V): Boolean = actrIsDefined(v, r.inverse)

  def partialActr(v: V, r: Relabeling): Nullbox[V] = if (actrIsDefined(v, r)) Nullbox(actr(v, r)) else Nullbox.empty[V]
  def partialActl(r: Relabeling, v: V): Nullbox[V] = partialActr(v, r.inverse)

  override def actr(v: V, r: Relabeling): V = {
    def newSymGrpOption: Option[Grp[Relabeling]] = v.symmetryGroupIfComputed.map(sym => sym.conjBy(InversePair(r, r.inverse)))
    v.representation match {
      case SPRepresentation | NPRepresentation =>
        implicit val action = v.scenario.probabilityAction
        v.builder(v.scenario, v.representation, v.coefficients <|+| r, newSymGrpOption)
      case WRepresentation =>
        implicit val action = v.scenario.strategyAction
        v.builder(v.scenario, v.representation, v.coefficients <|+| r, newSymGrpOption)
      case TRepresentation =>
        actr(v.to(WRepresentation), r).to(v.representation)
      case SCRepresentation | SGRepresentation =>
        actr(v.to(SPRepresentation), r).to(v.representation)
      case NCRepresentation | NGRepresentation =>
        actr(v.to(NPRepresentation), r).to(v.representation)
    }
  }
  override def actl(r: Relabeling, v: V): V = actr(v, r.inverse)
}
*/