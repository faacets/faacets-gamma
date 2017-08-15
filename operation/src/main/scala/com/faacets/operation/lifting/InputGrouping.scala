package com.faacets
package operation
package lifting

import scala.collection.mutable.{BitSet => MutableBitSet}

import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.perms.Perm
import net.alasc.perms.orbits.Points

/** Detection of outputs groupings.
  * 
  * For each input of each party, we find which outputs are grouped together,
  * and collect them into a `InputGrouping`.
  */
case class InputGrouping(partition: Partition) {

  val nOutputs = partition.size

  override def toString =
    if (partition.size == partition.nBlocks) partition.size.toString else (0 until partition.size).map(k => partition.blockIndex(k)).mkString("{", " ", "}")

  def isLiftedInput = partition.nBlocks == 1

  def hasLiftedOutputs = partition.blocks.exists(_.size > 1)

  def compact: Option[Int] = if (isLiftedInput) None else Some(partition.nBlocks)

}

object InputGrouping {

  /** Singleton groupings, i.e. the input does not contain a lifting. */
  def noLifting(nOutputs: Int): InputGrouping = InputGrouping(Partition((0 until nOutputs).map(Set(_)): _*))

  /** Only one grouping containing all outputs. */
  def full(nOutputs: Int): InputGrouping = InputGrouping(Partition((0 until nOutputs).toSet))

  /** Constructs a grouping from a symmetry group of output relabelings. */
  def apply(nOutputs: Int, inputSymSubgroup: Grp[Perm]): InputGrouping = {
    val gens = inputSymSubgroup.generators
    val remainingOutputs = MutableBitSet.empty ++= (0 until nOutputs)
    var groupings = Seq.empty[Set[Int]]
    while (!remainingOutputs.isEmpty) {
      val output = remainingOutputs.min
      val orbit = Points(output, gens)
      groupings = groupings :+ orbit
      remainingOutputs --= orbit
    }
    InputGrouping(Partition(groupings: _*))
    // TODO: add Partition.fromPermutations(Domain(nOutputs))(gens) in alasc
  }

}
