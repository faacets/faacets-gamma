package com.faacets
package core
package perm

import spire.algebra.PartialOrder
import spire.algebra.lattice._
import spire.syntax.eq._
import spire.syntax.partialOrder._
import spire.syntax.lattice._

import net.alasc.algebra._
import net.alasc.domains._
import net.alasc.wreath.Divisor
import net.alasc.syntax.all._

case class ShapeLattice(pm: PartitionMap[PartyShapeLattice]) {
  def n = pm.size
  def parties = (0 until n).map(pm(_).party).toSeq
  lazy val scenario = Scenario(parties)
  lazy val shape = scenario.shape
}

object ShapeLattice {
  def apply(parties: Seq[Party]): ShapeLattice =
    ShapeLattice(PartitionMap.tabulate(Partition.fromSeq(Domain(parties.length))(parties)) { block =>
      PartyShapeLattice(parties(block.min).inputs)
    })
  def apply(r: Relabeling): ShapeLattice =
    ShapeLattice(
      PartitionMap.tabulate(
        Partition.fromPermutation(Domain(r.nParties))(r.pPerm)
      )( block => (PartyShapeLattice.algebra.zero /: block) { case (jn, i) => jn join PartyShapeLattice(r.partyRelabeling(i)) } )
    )
  implicit val partitionMapPartyShapeLattice = PartitionMap.boundedBelowLatticeNonEmpty[PartyShapeLattice]
  implicit val partitionMapPartyShapePartialOrder = PartitionMap.partialOrder[PartyShapeLattice]

  implicit object algebra extends Lattice[ShapeLattice] with BoundedJoinSemilattice[ShapeLattice] with PartialOrder[ShapeLattice] {
    def zero = ShapeLattice(partitionMapPartyShapeLattice.zero)
    def join(x: ShapeLattice, y: ShapeLattice): ShapeLattice = ShapeLattice(x.pm join y.pm)
    def meet(x: ShapeLattice, y: ShapeLattice): ShapeLattice = ShapeLattice(x.pm meet y.pm)
    def partialCompare(x: ShapeLattice, y: ShapeLattice): Double = x.pm.partialCompare(y.pm)
  }
}