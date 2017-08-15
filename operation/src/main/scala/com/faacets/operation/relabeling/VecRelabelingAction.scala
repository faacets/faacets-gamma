package com.faacets
package operation
package relabeling
import spire.algebra.partial.PartialAction
import spire.math.Rational
import spire.syntax.group._
import spire.syntax.partialAction._
import spire.util.Opt
import scalin.immutable.Vec
import net.alasc.perms.default._

import com.faacets.core._

class VecRelabelingPartialAction[V <: PVec[V]](implicit builder: PVecBuilder[V]) extends PartialAction[V, Relabeling] {

  def partialActl(r: Relabeling, v: V): Opt[V] = partialActr(v, r.inverse)

  def partialActr(v: V, r: Relabeling): Opt[V] = {

    if (!v.scenario.group.contains(r)) Opt.empty[V]
    else {
      implicit def action: PartialAction[Vec[Rational], Relabeling] =
        com.faacets.data.instances.vec.vecPermutation[Rational, Relabeling](v.scenario.probabilityAction, implicitly)
      Opt(builder.updatedWithSymmetryGroup(v, v.scenario, (v.coefficients <|+|? r).get, g => Some(g.conjugatedBy(r))))
    }

  }

}
