package com.faacets.core

import spire.algebra.{Eq, Group}
import spire.laws.{ActionLaws, GroupLaws}
import com.faacets.FaacetsSuite
import com.faacets.laws._
import org.typelevel.discipline.Laws
import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.laws.{AnyRefLaws, Grps, PermutationActionLaws}

class RelabelingSuite extends FaacetsSuite {

  test("Identity are parsed correctly") {
    assert("".parseUnsafe[Relabeling] === Relabeling.id)
    assert("".parseUnsafe[PartyRelabeling] === PartyRelabeling.id)
  }

  locally {
    import Scenarios.Large._
    import Relabelings._
    checkAll("Relabeling", AnyRefLaws[Relabeling]._eq)
    checkAll("Relabeling", DataLaws[Relabeling].textable)
    checkAll("Relabeling", GroupLaws[Relabeling].group)
    implicit val eqSymbol: Eq[Symbol] = Eq.fromUniversalEquals[Symbol]
    import spire.std.tuples._
    import spire.std.int._
    checkAll("Relabeling", ActionLaws[Relabeling, (Symbol, Int, Int)].groupAction)
    forAll( (r: Relabeling) => r === (r.outputPart |+| r.inputPart |+| r.partyPart) )
    forAll( (r: Relabeling) => r === (r.outputInputPart |+| r.partyPart ) )
    forAll( (r: Relabeling) => r === Group[Relabeling].combineAll(r.components.map(_.get)) )
  }

  locally {
    import net.alasc.perms.default._
    import Scenarios.Small._
    import Relabelings.arbRelabeling
    import Grps.arbGrp
    checkAll("Grp[Relabeling]", DataLaws[Grp[Relabeling]].coded)
  }


  def relabelingLaws(rep: Scenario => PermutationAction[Relabeling])(implicit scenario: Scenario): Laws#RuleSet = {
    import Relabelings.arbRelabelingInScenario
    implicit def action = rep(scenario)
    PermutationActionLaws[Relabeling].faithfulPermutationAction
  }

  locally {
    import Scenarios.Small._

    nestedCheckAll[Scenario]("Relabeling.Marginal", Scenario.CHSH)(
      relabelingLaws(_.marginalAction)(_))

    nestedCheckAll[Scenario]("Relabeling.Probability", Scenario.CHSH)(
      relabelingLaws(_.probabilityAction)(_))

  }

  locally {
    import Scenarios.Tiny._

    nestedCheckAll[Scenario]("Relabeling.Strategy", Scenario.CHSH)(
      relabelingLaws(_.strategyAction)(_))

  }

  test("Action on triplets") {
    (('A, 0, 1) <|+| (rel"A0(0,1,2) A(0,2) (A,C)")) shouldBe (('C, 2, 2))
  }

}
