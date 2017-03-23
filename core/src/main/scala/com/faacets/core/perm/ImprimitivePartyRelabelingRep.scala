package com.faacets
package core
package perm

import spire.syntax.lattice._

import net.alasc.algebra.PermutationAction
import net.alasc.finite.FaithfulPermutationActionBuilder

final class ImprimitivePartyRelabelingRepBuilder extends FaithfulPermutationActionBuilder[PartyRelabeling] {

  def apply(generators: Iterable[PartyRelabeling]): PermutationAction[PartyRelabeling] =
    generators.foldLeft(PartyShapeLattice.algebra.zero)(_ join PartyShapeLattice(_)).shape.ImprimitiveAction

}
