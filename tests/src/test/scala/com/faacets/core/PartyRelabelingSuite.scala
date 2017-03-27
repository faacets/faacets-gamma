package com.faacets.core

import spire.laws.GroupLaws

import com.faacets.FaacetsSuite
import com.faacets.laws._
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import net.alasc.algebra.PermutationAction
import net.alasc.laws.{AnyRefLaws, PermutationActionLaws}

class PartyRelabelingSuite extends FaacetsSuite {

  {
    import Parties.Large._
    import PartyRelabelings._

    checkAll("PartyRelabeling", AnyRefLaws[PartyRelabeling]._eq)
    checkAll("PartyRelabeling", DataLaws[PartyRelabeling].textable)
    checkAll("PartyRelabeling", GroupLaws[PartyRelabeling].group)
  }

  def partyRelabelingLaws(rep: Party => PermutationAction[PartyRelabeling])(implicit party: Party): Laws#RuleSet = {
    import PartyRelabelings.arbPartyRelabelingInParty
    implicit def action = rep(party)
    PermutationActionLaws[PartyRelabeling].faithfulPermutationAction
  }

  {
    import Parties.Small._

    nestedCheckAll[Party]("Relabeling.Probability", Party.binaryIO)(
      partyRelabelingLaws(_.probabilityAction)(_))

    nestedCheckAll[Party]("Relabeling.Strategy", Party.binaryIO)(
      partyRelabelingLaws(_.strategyAction)(_))

  }

}
