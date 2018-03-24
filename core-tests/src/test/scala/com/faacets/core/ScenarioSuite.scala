package com.faacets
package core

import com.faacets.laws.DataLaws
import net.alasc.laws.AnyRefLaws

class ScenarioSuite extends FaacetsSuite {

  import com.faacets.laws.Scenarios.Large._
  import com.faacets.laws.Scenarios.{scenarioCloner, scenarioInstances}

  checkAll("Scenario", DataLaws[Scenario].textable)

  checkAll("Scenario", AnyRefLaws[Scenario]._eq)

  test("Parse scenario with whitespace") {
    assert("[   (2  2)    (2   2)   ]".parseUnsafe[Scenario] === Scenario.CHSH)
  }

}
