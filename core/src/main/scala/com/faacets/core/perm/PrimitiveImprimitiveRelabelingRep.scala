package com.faacets
package core
package perm

/*
import spire.algebra.Ring
import spire.syntax.lattice._
import spire.syntax.cfor._
import spire.syntax.action._

import net.alasc.perms.FaithfulPermRep

case class PrimitiveImprimitiveRelabelingRep[K](lattice: ShapeLattice)(implicit val scalar: Ring[K])
  extends FaithfulPermRep[Relabeling, K] {

  def dimension = lattice.shape.primitiveImprimitive.size

  type F = lattice.shape.PriImpAction.type

  val permutationAction: F = lattice.shape.PriImpAction

  def represents(r: Relabeling): Boolean = lattice.shape.represents(r) && {
    cforRange(0 until lattice.shape.parties.size) { p =>
      if (lattice.shape.imprimitiveSizes(p) == 1 && (p <|+| r.pPerm) != p) return false
    }
    true
  }

}

object PrimitiveImprimitiveRelabelingRep {

  def apply[K](generators: Iterable[Relabeling])(implicit K: Ring[K]): PrimitiveImprimitiveRelabelingRep[K] = {
    val lattice = generators.foldLeft(ShapeLattice.algebra.zero)(_ join ShapeLattice(_))
    apply[K](lattice)
  }

}
*/