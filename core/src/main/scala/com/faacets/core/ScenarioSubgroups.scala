package com.faacets
package core

import spire.algebra.Group
import spire.math.fact

import net.alasc.domains.{Domain, Partition}
import net.alasc.finite.Grp
import net.alasc.perms.{GrpFixingPartition, Perm}
import net.alasc.perms.default._
import net.alasc.syntax.all._

import Select._

import spire.math.SafeLong

case class ScenarioSubgroups(scenario: Scenario, permuteSingleInputOutputParties: Boolean = true,
                             permuteSingleOutputInputs: Boolean = true) {

  def n = scenario.parties.length
  def isSingleIOParty(p: Int): Boolean = scenario.parties(p).inputs.size == 1 && scenario.parties(p).inputs(0) == 1

  val singleInputOutputParties: Set[Int] = (0 until n).toSet.filter(isSingleIOParty)
  val nSingleInputOutputParties = singleInputOutputParties.size
  val partition: Partition = Partition.fromSeq(Domain(n))(scenario.parties)

  def innerGenerators: IndexedSeq[Relabeling] =
    if (permuteSingleOutputInputs)
      (0 until n).flatMap { p => scenario.parties(p).subgroups.generators.map(_.forParty(p)) }
    else
      (0 until n).flatMap { p => scenario.parties(p).strategySubgroups.generators.map(_.forParty(p)) }

  def innerOrder: SafeLong =
    if (permuteSingleOutputInputs)
      (0 until n).map(p => scenario.parties(p).subgroups.order).reduce(_*_)
    else
      (0 until n).map(p => scenario.parties(p).strategySubgroups.order).reduce(_*_)

  def partyOrder: SafeLong = {
    val fullOrder = GrpFixingPartition.order(partition)
    if (permuteSingleInputOutputParties) fullOrder else fullOrder / fact(nSingleInputOutputParties)
  }
  def partyGenerators: IndexedSeq[Relabeling] = {
    val fullGenerators = GrpFixingPartition.generators(partition)
    val generators = if (permuteSingleInputOutputParties) fullGenerators
    else fullGenerators.filterNot(g => isSingleIOParty(g.findMovedPoint.get))
    generators.map(p => Relabeling.PartyComponent(p).get)
  }
  def partyPermGroup: Grp[Perm] = {
    val fullGroup = GrpFixingPartition(partition)
    if (permuteSingleInputOutputParties) fullGroup else fullGroup.pointwiseStabilizer(singleInputOutputParties)
  }
  def partyGroup: Grp[Relabeling] = Grp.fromGeneratorsAndOrder(partyGenerators, partyOrder)

  def generators: IndexedSeq[Relabeling] = innerGenerators ++ partyGenerators
  def order: SafeLong = innerOrder * partyOrder

  def apply(outputs: Select, inputs: Select, parties: Select): Iterable[Grp[Relabeling]] =
    (outputs, inputs, parties) match {
      case (_, _, For(party)) =>
        scenario.parties(party).subgroups(outputs, inputs).map { partySubgroup =>
          val generators = partySubgroup.generators.map(pg => Relabeling(Map(party -> pg), Group[Perm].id))

          Grp.fromGeneratorsAndOrder(generators, partySubgroup.order)
        }
      case (_, _, Every) =>
        (0 until n).flatMap { p =>
          apply(outputs, inputs, For(p))
        }
      case (_, _, NotPermuted) =>
        val partySubgroups = (0 until n).flatMap { p =>
          apply(outputs, inputs, For(p))
        }
        val generators = partySubgroups.flatMap(_.generators)
        val order = partySubgroups.map(_.order).reduce(_*_)
        Iterable(Grp.fromGeneratorsAndOrder(generators, order))
      case (_, _, Permuted) =>
        val Seq(partySubgroup) = apply(outputs, inputs, NotPermuted)
        val ioGenerators = partySubgroup.generators
        val ioOrder = partySubgroup.order
        Iterable(Grp.fromGeneratorsAndOrder(ioGenerators ++ partyGenerators, ioOrder * partyOrder))
      case _ => sys.error(s"Bell subgroup for outputs = $outputs, inputs = $inputs and parties = $parties not supported")
    }
  
}
