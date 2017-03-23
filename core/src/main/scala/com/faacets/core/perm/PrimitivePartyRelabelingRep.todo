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
import spire.syntax.cfor._
import spire.syntax.lattice._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.perms.{FaithfulPermRep, FaithfulPermRepBuilder, PermRep}
import net.alasc.perms.default._

case class PrimitivePartyRelabelingRep[K](lattice: PartyShapeLattice)(implicit val scalar: Ring[K])
  extends FaithfulPermRep[PartyRelabeling, K] {

  def dimension = lattice.shape.primitive.size

  type F = lattice.shape.PrimitiveAction.type

  val permutationAction: F = lattice.shape.PrimitiveAction

  def represents(pr: PartyRelabeling): Boolean = lattice.shape.represents(pr) && {
    cforRange(0 until lattice.shape.inputs.size) { i =>
      if (lattice.shape.inputs(i) == 1 && (i <|+| pr.xPerm) != i) return false
    }
    true
  }

}

object PrimitivePartyRelabelingRep {

  def apply[K](generators: Iterable[PartyRelabeling])(implicit K: Ring[K]): PrimitivePartyRelabelingRep[K] = {
    val lattice = generators.foldLeft(PartyShapeLattice.algebra.zero)(_ join PartyShapeLattice(_))
    apply[K](lattice)
  }

}
*/