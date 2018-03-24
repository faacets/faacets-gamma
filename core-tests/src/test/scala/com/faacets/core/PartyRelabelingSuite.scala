package com.faacets.core

import com.faacets.FaacetsSuite
import com.faacets.laws._
import net.alasc.algebra.PermutationAction
import net.alasc.laws.{AnyRefLaws, PermutationActionLaws}
import org.typelevel.discipline.Laws
import spire.algebra.Group
import spire.laws.{ActionLaws, GroupLaws}

class PartyRelabelingSuite extends FaacetsSuite {

  {
    import Parties.Large._
    import PartyRelabelings._

    checkAll("PartyRelabeling", AnyRefLaws[PartyRelabeling]._eq)
    checkAll("PartyRelabeling", DataLaws[PartyRelabeling].textable)
    checkAll("PartyRelabeling", GroupLaws[PartyRelabeling].group)

    import spire.std.int._
    import spire.std.tuples._
    checkAll("PartyRelabeling", ActionLaws[PartyRelabeling, (Int, Int)].groupAction)
    forAll( (pr: PartyRelabeling) => pr === (pr.outputPart |+| pr.inputPart) )
    forAll( (pr: PartyRelabeling) => pr === (Group[PartyRelabeling].combineAll(pr.components.map(_.get))) )

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

  test("Action on pairs") {
    ((0, 1) <|+| (prel"0(0,1,2) I(0,2)")) shouldBe ((2, 2))
  }

}
