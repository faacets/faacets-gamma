package com.faacets
package core
package perm

import spire.algebra.PartialOrder
import spire.algebra.lattice._
import spire.syntax.eq._
import spire.syntax.action._
import spire.syntax.lattice._
import spire.syntax.partialOrder._

import net.alasc.algebra._
import net.alasc.domains._
import net.alasc.wreath.Divisor
import net.alasc.syntax.all._

case class PartyShapeLattice(pm: PartitionMap[Int]) {
  def n = pm.size
  def inputs = (0 until n).map(pm(_)).toSeq
  lazy val party = Party(inputs)
  lazy val shape = party.shape
}

object PartyShapeLattice {

  implicit object NumOutputsLattice extends Lattice[Int] with BoundedJoinSemilattice[Int] with PartialOrder[Int] {
    def zero = 2
    def join(x: Int, y: Int) = x.max(y)
    def meet(x: Int, y: Int) = x.min(y)
    def partialCompare(x: Int, y: Int): Double = ((x - y).signum).toDouble
  }

  implicit val partitionMapIntLattice = PartitionMap.boundedBelowLatticeNonEmpty[Int]

  implicit val partitionMapIntPartialOrder = PartitionMap.partialOrder[Int]

  def apply(pr: PartyRelabeling): PartyShapeLattice =
    PartyShapeLattice(
      PartitionMap.tabulate(
        Partition.fromPermutation(Domain(pr.nInputs))(pr.xPerm)
      )( block => (2 /: block) { case (mx, i) => mx.max(pr.aPerm(i).largestMovedPoint.getOrElseFast(0) + 1) } )
    )

  def apply(inputs: Seq[Int]): PartyShapeLattice =
    PartyShapeLattice(PartitionMap.tabulate(Partition.fromSeq(Domain(inputs.size))(inputs)) { block =>
      inputs(block.min)
    })

  implicit object algebra extends Lattice[PartyShapeLattice] with BoundedJoinSemilattice[PartyShapeLattice] with PartialOrder[PartyShapeLattice] {
    def zero = PartyShapeLattice(partitionMapIntLattice.zero)
    def join(x: PartyShapeLattice, y: PartyShapeLattice): PartyShapeLattice = PartyShapeLattice(x.pm join y.pm)
    def meet(x: PartyShapeLattice, y: PartyShapeLattice): PartyShapeLattice = PartyShapeLattice(x.pm meet y.pm)
    def partialCompare(x: PartyShapeLattice, y: PartyShapeLattice): Double = x.pm.partialCompare(y.pm)
  }

}
