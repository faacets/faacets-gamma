package com.faacets
package core
package perm

import scala.annotation.tailrec

import net.alasc.algebra._
import net.alasc.finite.FaithfulPermutationActionBuilder

final class ImprimitiveImprimitiveRelabelingRepBuilder extends FaithfulPermutationActionBuilder[Relabeling] {

  def apply(generators: Iterable[Relabeling]): PermutationAction[Relabeling] = {
    @tailrec def iter(it: Iterator[Relabeling], maxA: Int = 0, maxX: Int = 0, maxP: Int = 0): Scenario =
      if (it.hasNext) {
        val r = it.next()
        val newMaxA = spire.math.max(r.nOutputsMax, maxA)
        val newMaxX = spire.math.max(r.nInputsMax, maxX)
        val newMaxP = spire.math.max(r.nParties, maxP)
        iter(it, newMaxA, newMaxX, newMaxP)
      } else
        Scenario.nmk(maxP + 1, maxX + 1, maxA + 1)

    iter(generators.iterator).shape.ImpImpAction
  }

}
