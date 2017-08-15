package com.faacets
package core

import spire.math.SafeLong

import net.alasc.partitions.Partition
import net.alasc.finite.Grp
import net.alasc.perms.{GrpFixingPartition, Perm}
import net.alasc.syntax.all._
import net.alasc.perms.default._
import spire.math.fact

import Select._

case class PartySubgroups(party: Party, permuteSingleOutputInputs: Boolean = true) {

  def n = party.inputs.length
  val singleOutputInputs: Set[Int] = (0 until n).toSet.filter(party.inputs(_) == 1)
  val nSingleOutputInputs = singleOutputInputs.size

  val partition: Partition = Partition.fromSeq(party.inputs)

  def outputGenerators(x: Int): IndexedSeq[PartyRelabeling] =
    (0 until (party.inputs(x) - 1)).map(a => PartyRelabeling.OutputComponent(x, Perm(a, a + 1)).get)
  def outputOrder(x: Int): SafeLong = fact(party.inputs(x))
  def outputGroup(x: Int): Grp[PartyRelabeling] =
    Grp.fromGeneratorsAndOrder(outputGenerators(x).toIndexedSeq, outputOrder(x))

  def allOutputsOrder: SafeLong = (0 until n).map(outputOrder(_)).reduce(_*_)
  def allOutputsGenerators: IndexedSeq[PartyRelabeling] =
    (0 until n).flatMap(outputGenerators(_))
  def allOutputsGroup: Grp[PartyRelabeling] =
    Grp.fromGeneratorsAndOrder(allOutputsGenerators.toIndexedSeq, allOutputsOrder)

  def inputOrder: SafeLong = {
    val fullOrder = GrpFixingPartition.order(partition)
    if (permuteSingleOutputInputs) fullOrder else fullOrder / fact(nSingleOutputInputs)
  }
  def inputGenerators: IndexedSeq[PartyRelabeling] = {
    val fullGenerators = GrpFixingPartition.generators(partition)
    val generators = if (permuteSingleOutputInputs) fullGenerators
      else fullGenerators.filterNot(g => party.inputs(g.findMovedPoint.get) == 1)
    generators.map(x => PartyRelabeling.InputComponent(x).get)
  }
  def inputPermGroup: Grp[Perm] = {
    val fullGroup = GrpFixingPartition(partition)
    if (permuteSingleOutputInputs) fullGroup else fullGroup.pointwiseStabilizer(singleOutputInputs)
  }
  def inputGroup: Grp[PartyRelabeling] = Grp.fromGeneratorsAndOrder(inputGenerators, inputOrder)
  def generators = allOutputsGenerators ++ inputGenerators
  def order = allOutputsOrder * inputOrder

  def apply(outputs: Select, inputs: Select): Seq[Grp[PartyRelabeling]] =
    (outputs, inputs) match {
      case (Permuted, Permuted) => Seq(party.group)
      case (Permuted, For(input)) => Seq(outputGroup(input))
      case (_, Every) => (0 until n).flatMap { input => apply(outputs, For(input)) }
      case (Permuted, NotPermuted) => Seq(allOutputsGroup) // output permutations
      case (NotPermuted, Permuted) =>  Seq(inputGroup) // input permutations
      case (NotPermuted, NotPermuted) => Seq(Grp.trivial[PartyRelabeling])
      case _ => sys.error(s"Party subgroup for outputs = $outputs and inputs = $inputs not supported")
    }

}
