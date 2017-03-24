package com.faacets
package core
package perm

import spire.syntax.lattice._

import net.alasc.algebra._
import net.alasc.finite.FaithfulPermutationActionBuilder

final class ImprimitiveImprimitiveRelabelingRepBuilder extends FaithfulPermutationActionBuilder[Relabeling] {

  def apply(generators: Iterable[Relabeling]): PermutationAction[Relabeling] =
    generators.foldLeft(ShapeLattice.algebra.zero)(_ join ShapeLattice(_)).shape.ImpImpAction

}