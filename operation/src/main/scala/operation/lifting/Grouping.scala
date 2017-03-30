package com.faacets
package operation
package lifting
/*
import spire.algebra.Eq

import net.alasc.math.Grp

import core._
import core.perm._
import data._

case class Grouping(parties: Seq[PartyGrouping]) {
  override def toString = parties.mkString("[", " ", "]")
  def nParties: Int = parties.size
  def scenario: Scenario = Scenario(parties.map(_.party).toVector)

  def hasLiftedOutputs: Boolean = parties.exists(_.hasLiftedOutputs)
  def hasLiftedInputs: Boolean = parties.exists(_.hasLiftedInputs)
  def isLifting: Boolean = hasLiftedOutputs || hasLiftedInputs

  def compact = Grouping(parties.map(_.compact))

  def updated(p: Int, partyGrouping: PartyGrouping): Grouping = Grouping(parties.updated(p, partyGrouping))
}

object Grouping {
  def apply(scenario: Scenario, symSubgroup: Grp[Relabeling]): Grouping = {
    val trivialGrp = Grp.trivial[PartyRelabeling]
    val subgroups = symSubgroup.partyRelabelingSubgroups
    val partyGroupings = scenario.parties.indices.map( p => PartyGrouping(scenario.parties(p), subgroups.getOrElse(p, trivialGrp)) )
    Grouping(partyGroupings)
  }

  def apply(expr: Expr): Grouping = expr.representation match {
    case TRepresentation | WRepresentation =>
      throw new IllegalArgumentException("Grouping not supported for strategies.")
    case NPRepresentation | SPRepresentation =>
      val symSG = expr.symmetryGroup
      apply(expr.scenario, symSG)
    case _ =>
      apply(expr.to(expr.representation.permutable))
  }

  implicit val Parsable: Parsable[Grouping] = new GroupingParsable
  implicit object Eq extends Eq[Grouping] {
    def eqv(x: Grouping, y: Grouping) = (x == y)
  }
}
*/