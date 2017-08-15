package com.faacets
package core
package perm

import scala.annotation.tailrec

import spire.syntax.lattice._
import net.alasc.algebra.PermutationAction
import net.alasc.finite.FaithfulPermutationActionBuilder

final class ImprimitivePartyRelabelingRepBuilder extends FaithfulPermutationActionBuilder[PartyRelabeling] {

  def apply(generators: Iterable[PartyRelabeling]): PermutationAction[PartyRelabeling] = {
    @tailrec def iter(it: Iterator[PartyRelabeling], maxA: Int = 0, maxX: Int = 0): Party =
      if (it.hasNext) {
        val pr = it.next()
        val newMaxA = spire.math.max(pr.nOutputs, maxA)
        val newMaxX = spire.math.max(pr.nInputs, maxX)
        iter(it, newMaxA, newMaxX)
      } else
        Party.mk(maxX + 1, maxA + 1)

    iter(generators.iterator).shape.ImprimitiveAction
  }

}
