package com.faacets.core

import spire.laws.GroupLaws
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

}
