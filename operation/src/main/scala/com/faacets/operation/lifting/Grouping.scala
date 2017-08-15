package com.faacets
package operation
package lifting

import spire.algebra.Eq
import net.alasc.finite.Grp

import com.faacets.core._
import com.faacets.core.syntax.subgroups._
import com.faacets.data._

case class Grouping(parties: Seq[PartyGrouping]) {

  override def toString = parties.mkString("[", " ", "]")

  def nParties: Int = parties.size

  def scenario: Scenario = Scenario(parties.map(_.party).toVector)

  def hasLiftedOutputs: Boolean = parties.exists(_.hasLiftedOutputs)

  def hasLiftedInputs: Boolean = parties.exists(_.hasLiftedInputs)

  def isLifting: Boolean = hasLiftedOutputs || hasLiftedInputs

  def minimalScenario: Scenario = Scenario(parties.map(_.minimalParty))

  def compactOutputs: Grouping = Grouping(parties.map(_.compactOutputs))

  def updated(p: Int, partyGrouping: PartyGrouping): Grouping = Grouping(parties.updated(p, partyGrouping))

}

object Grouping {

  def apply(scenario: Scenario, symSubgroup: Grp[Relabeling]): Grouping = {
    val trivialGrp = Grp.trivial[PartyRelabeling]
    val subgroups = symSubgroup.partyRelabelingSubgroups
    val partyGroupings = scenario.parties.indices.map( p => PartyGrouping(scenario.parties(p), subgroups.getOrElse(p, trivialGrp)) )
    Grouping(partyGroupings)
  }

  def apply(expr: Expr): Grouping = apply(expr.scenario, expr.symmetryGroup)

  def noLifting(scenario: Scenario): Grouping = Grouping(scenario.parties.map(PartyGrouping.noLifting(_)))

  implicit val textable: Textable[Grouping] = Textable.fromParser(GroupingParsers.grouping, _.toString)

  implicit val equ: Eq[Grouping] = Eq.fromUniversalEquals[Grouping]

}
