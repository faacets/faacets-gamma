package com.faacets
package core
package perm

import net.alasc.algebra.PermutationAction
import net.alasc.finite.FaithfulPermutationActionBuilder

final class ImprimitivePartyRelabelingRepBuilder extends FaithfulPermutationActionBuilder[PartyRelabeling] {

  def apply(generators: Iterable[PartyRelabeling]): PermutationAction[PartyRelabeling] =
    Party.homogenousFor(generators).shape.ImprimitiveAction

}
