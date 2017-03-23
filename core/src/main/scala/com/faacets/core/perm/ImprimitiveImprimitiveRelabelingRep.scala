package com.faacets
package core
package perm
/*
import scala.reflect.classTag

import spire.algebra._
import spire.algebra.lattice._
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.lattice._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.perms.{FaithfulPermRep, FaithfulPermRepBuilder}

case class ImprimitiveImprimitiveRelabelingRep[K](lattice: ShapeLattice)(implicit val scalar: Ring[K])
  extends FaithfulPermRep[Relabeling, K] {

  type F = lattice.shape.ImpImpAction.type

  def dimension = lattice.shape.imprimitiveImprimitive.size

  val permutationAction: F = lattice.shape.ImpImpAction

  def represents(r: Relabeling) = lattice.shape.represents(r)

}

object ImprimitiveImprimitiveRelabelingRep {

  def apply[K](generators: Iterable[Relabeling])(implicit K: Ring[K]): ImprimitiveImprimitiveRelabelingRep[K] = {
    val lattice = generators.foldLeft(ShapeLattice.algebra.zero)(_ join ShapeLattice(_))
    apply[K](lattice)
  }

}

final class ImprimitiveImprimitiveRelabelingRepBuilder extends FaithfulPermRepBuilder[Relabeling] {

  def build[K](generators: Iterable[Relabeling])(implicit K: Ring[K]): FaithfulPermRep[Relabeling, K] =
    ImprimitiveImprimitiveRelabelingRep(generators)

}
*/