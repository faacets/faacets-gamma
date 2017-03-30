package com.faacets
package operation
package lifting
/*
import scala.collection.immutable
import scala.collection.mutable

import spire.syntax.action._

import net.alasc.math.{Grp, Perm, Domain}

import core._
import util._
import perm.{Relabeling, PartyRelabeling}

/** Detection of outputs groupings.
  * 
  * For each input of each party, we find which outputs are grouped together,
  * and collect them into a `InputGrouping`.
  */
case class InputGrouping(partition: Domain#Partition) {
  val nOutputs = partition.size
  override def toString =
    if (partition.size == partition.nBlocks) partition.size.toString else (0 until partition.size).map(k => partition.blockIndex(k)).mkString("{", " ", "}")

  def isLiftedInput = partition.nBlocks == 1
  def hasLiftedOutputs = partition.blocks.exists(_.size > 1)
  def compact: Option[InputGrouping] = if (isLiftedInput) None else Some(InputGrouping.noLifting(partition.nBlocks))
}

object InputGrouping {
  def noLifting(nOutputs: Int): InputGrouping = InputGrouping(Domain.Partition((0 until nOutputs).map(Set(_)): _*))
  def full(nOutputs: Int): InputGrouping = InputGrouping(Domain.Partition((0 until nOutputs).toSet))
  def apply(nOutputs: Int, inputSymSubgroup: Grp[Perm]): InputGrouping = {
    val gens = inputSymSubgroup.generators
    val remainingOutputs = mutable.BitSet.empty ++= (0 until nOutputs)
    var groupings = Seq.empty[immutable.BitSet]
    while (!remainingOutputs.isEmpty) {
      import net.alasc.math.OrbitInstances._
      val output = remainingOutputs.min
      val orbit = immutable.BitSet(output) <|+| (gens: Iterable[Perm])
      groupings = groupings :+ orbit
      remainingOutputs --= orbit
    }
    InputGrouping(Domain.Partition(groupings: _*))
  }
}
*/