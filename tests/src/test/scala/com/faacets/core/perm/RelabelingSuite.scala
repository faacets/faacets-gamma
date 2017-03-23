package com.faacets
package core
package perm

import spire.laws.GroupLaws

import com.faacets.laws._
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.Laws

import net.alasc.laws.{AnyRefLaws, Dom, PermutationActionLaws}

class RelabelingSuite extends FaacetsSuite {

  import Scenarios.Large._

  {
    import Relabelings._

    checkAll("Relabeling", AnyRefLaws[Relabeling]._eq)
    checkAll("Relabeling", DataLaws[Relabeling].textable)
    checkAll("Relabeling", GroupLaws[Relabeling].group)
  }

  /*
  def relabelingLaws(rep: Scenario => PermRep[Relabeling, _])(implicit scenario: Scenario): Laws#RuleSet = {
    import Relabelings.arbRelabelingInScenario
    def r = rep(scenario)
    implicit def action = r.permutationAction
    implicit def arbDom: Arbitrary[Dom] = Arbitrary { Gen.choose(0, r.dimension).map(Dom(_)) }
    PermutationActionLaws[Relabeling].faithfulPermutationAction
  }*/

  /*

  // missing Relabeling, PartyRelabeling from parts // hashCode
  {
    import Scenarios.Small._

    nestedCheckAll[Scenario]("Relabeling.Marginal", Scenario.CHSH)(
      relabelingLaws(_.marginalRep)(_))

    nestedCheckAll[Scenario]("Relabeling.Probability", Scenario.CHSH)(
      relabelingLaws(_.probabilityRep)(_))

  }

  {
    import Scenarios.Tiny._

    nestedCheckAll[Scenario]("Relabeling.Strategy", Scenario.CHSH)(
      relabelingLaws(_.strategyRep)(_))

  }

*/
}
