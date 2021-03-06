package com.faacets
package operation
package lifting

import net.alasc.finite.Grp
import net.alasc.perms.Perm

import com.faacets.core._
import com.faacets.core.syntax.subgroups._

case class PartyGrouping(inputs: Seq[InputGrouping]) {

  override def toString = inputs.mkString("(", " ", ")")

  def party: Party = Party(inputs.map(_.nOutputs).toVector)

  def hasLiftedOutputs: Boolean = inputs.exists(_.hasLiftedOutputs)

  def hasLiftedInputs: Boolean = inputs.exists(_.isLiftedInput)

  def isLifting: Boolean = hasLiftedOutputs || hasLiftedInputs

  def minimalParty: Party = Party(inputs.flatMap(_.compact))

  def compactOutputs: PartyGrouping = PartyGrouping(inputs.map(ig => InputGrouping.noLifting(ig.partition.nBlocks)))

  lazy val indexFromCompact: Seq[Int] = inputs.zipWithIndex.filter(!_._1.isLiftedInput).map(_._2)

  lazy val compactFromIndex: Seq[Option[Int]] = (0 until inputs.length).map(x => indexFromCompact.zipWithIndex.find(_._1 == x).map(_._2))

  def updated(x: Int, inputGrouping: InputGrouping): PartyGrouping = PartyGrouping(inputs.updated(x, inputGrouping))

}

object PartyGrouping {

  def apply(party: Party, partySymSubgroup: Grp[PartyRelabeling]): PartyGrouping = {
    val trivialGrp = Grp.trivial[Perm]
    val subgroups = partySymSubgroup.outputPermSubgroups
    val inputGroupings = party.inputs.indices.map( x => InputGrouping(party.inputs(x), subgroups.getOrElse(x, trivialGrp)) )
    PartyGrouping(inputGroupings)
  }

  def noLifting(party: Party): PartyGrouping = PartyGrouping(party.inputs.map(InputGrouping.noLifting(_)))

}
