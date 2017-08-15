package com.faacets
package operation
package relabeling
import com.faacets.core.perm.ShapeLattice
import spire.syntax.group._
import spire.syntax.order._
import spire.syntax.partialAction._
import core._
import scalin.immutable.Vec
import spire.algebra.partial.PartialAction
import spire.math.Rational
import spire.util.Opt
import net.alasc.perms.default._
import net.alasc.perms.orbits.Seqs

class VecRelabelingPartialAction[V <: PVec[V]](implicit builder: PVecBuilder[V]) extends PartialAction[V, Relabeling] {

  def partialActl(r: Relabeling, v: V): Opt[V] = partialActr(v, r.inverse)

  def partialActr(v: V, r: Relabeling): Opt[V] = {
    val rLattice = ShapeLattice(r)

    if (!(rLattice <= v.scenario.shapeLattice)) Opt.empty[V]
    else {
      import scalin.immutable.dense._
      implicit def action: PartialAction[Vec[Rational], Relabeling] =
        com.faacets.data.instances.vec.vecPermutation[Rational, Relabeling](v.scenario.probabilityAction, implicitly)
      Opt(builder.updatedWithSymmetryGroup(v, v.scenario, (v.coefficients <|+|? r).get, g => Some(g.conjugatedBy(r))))
    }

  }

}
