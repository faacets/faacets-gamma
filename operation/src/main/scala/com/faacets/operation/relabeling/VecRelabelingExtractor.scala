package com.faacets
package operation
package relabeling

import spire.algebra.partial.PartialAction
import spire.util.Opt
import net.alasc.perms.default._
import net.alasc.perms.orbits.Seqs

import com.faacets.core._

class VecRelabelingExtractor[V <: NDVec[V]](implicit val partialAction: PartialAction[V, Relabeling]) extends GroupOperationExtractor[V, Relabeling] {

  def group = Relabeling.group

  def extractOperation(e: V): Opt[Relabeling] = {
    val r = Seqs.Representatives.ordered(e.scenario.group, e.scenario.probabilityAction, e.coefficients.toIndexedSeq, Opt(e.symmetryGroup)).minimum
    if (e.symmetryGroup.contains(r)) Opt.empty[Relabeling] else Opt(r)
  }

}
