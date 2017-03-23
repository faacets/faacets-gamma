package com.faacets
package core
package perm

/*
import spire.algebra._
import spire.algebra.lattice._
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.lattice._

import net.alasc.perms.{FaithfulPermRep, FaithfulPermRepBuilder}


case class ImprimitivePartyRelabelingRep[K](lattice: PartyShapeLattice)(implicit val scalar: Ring[K])
  extends FaithfulPermRep[PartyRelabeling, K]  {

  type F = lattice.shape.ImprimitiveAction.type

  def dimension = lattice.shape.imprimitive.size

  val permutationAction: F = lattice.shape.ImprimitiveAction

  def represents(pr: PartyRelabeling): Boolean = lattice.shape.represents(pr)

}

object ImprimitivePartyRelabelingRep {

  def apply[K](generators: Iterable[PartyRelabeling])(implicit K: Ring[K]): ImprimitivePartyRelabelingRep[K] = {
    val lattice = generators.foldLeft(PartyShapeLattice.algebra.zero)(_ join PartyShapeLattice(_))
    apply[K](lattice)
  }

}

final class ImprimitivePartyRelabelingRepBuilder extends FaithfulPermRepBuilder[PartyRelabeling] {

  def build[K](generators: Iterable[PartyRelabeling])(implicit K: Ring[K]): FaithfulPermRep[PartyRelabeling, K] =
    ImprimitivePartyRelabelingRep(generators)

}
*/