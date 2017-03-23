package com.faacets
package core.perm

import spire.laws.GroupLaws

import com.faacets.core.Party
import com.faacets.laws.PartyRelabelings.genPartyRelabeling
import laws._
import org.scalacheck.Arbitrary

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

  {

    val party = Party.mk(4, 4)

    implicit val ff: PermutationAction[PartyRelabeling] = party.probabilityAction

    implicit val pr: Arbitrary[PartyRelabeling] = PartyRelabelings.arbPartyRelabelingInParty(party)

    checkAll("PermutationAction[PartyRelabeling] (probability)", PermutationActionLaws[PartyRelabeling].faithfulPermutationAction)

  }


  {

    val party = Party.mk(4, 4)

    implicit val ff: PermutationAction[PartyRelabeling] = party.strategyAction

    implicit val pr: Arbitrary[PartyRelabeling] = PartyRelabelings.arbPartyRelabelingInParty(party)

    checkAll("PermutationAction[PartyRelabeling] (strategy)", PermutationActionLaws[PartyRelabeling].permutationAction)

  }


}
