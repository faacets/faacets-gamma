package com.faacets.core

import spire.laws.GroupLaws
import com.faacets.FaacetsSuite
import com.faacets.laws._
import org.typelevel.discipline.Laws
import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.laws.{AnyRefLaws, Grps, PermutationActionLaws}

class RelabelingSuite extends FaacetsSuite {


  {
    import Scenarios.Large._
    import Relabelings._

    checkAll("Relabeling", AnyRefLaws[Relabeling]._eq)
    checkAll("Relabeling", DataLaws[Relabeling].textable)
    checkAll("Relabeling", GroupLaws[Relabeling].group)
  }

  /*{ TODO
    import net.alasc.perms.default._
    import com.faacets.data.instances.grp._
    import Scenarios.Small._
    import Relabelings.arbRelabeling
    import Grps.arbGrp
    checkAll("Grp[Relabeling]", DataLaws[Grp[Relabeling]].coded)
  }*/


  def relabelingLaws(rep: Scenario => PermutationAction[Relabeling])(implicit scenario: Scenario): Laws#RuleSet = {
    import Relabelings.arbRelabelingInScenario
    implicit def action = rep(scenario)
    PermutationActionLaws[Relabeling].faithfulPermutationAction
  }

  {
    import Scenarios.Small._

    nestedCheckAll[Scenario]("Relabeling.Marginal", Scenario.CHSH)(
      relabelingLaws(_.marginalAction)(_))

    nestedCheckAll[Scenario]("Relabeling.Probability", Scenario.CHSH)(
      relabelingLaws(_.probabilityAction)(_))

  }

  {
    import Scenarios.Tiny._

    nestedCheckAll[Scenario]("Relabeling.Strategy", Scenario.CHSH)(
      relabelingLaws(_.strategyAction)(_))

  }

}
