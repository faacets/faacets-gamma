package com.faacets
package operation

import com.faacets.laws.Groupings
import com.faacets.operation.lifting.Grouping
import core._

/** Test suite for `Lifting`
  *
  * We check that liftings are correctly serialized to text and can be parsed back.
  *
  * We also check that the two examples given in Pironio 2005 (doi:10.1063/1.1928727) are
  * lifted correctly.
  */
class LiftingSuite extends FaacetsSuite {

  val scenario_CHSH = "[(2 2) (2 2)]".parseUnsafe[Scenario]
  val scenario_222_222 = "[(2 2 2) (2 2 2)]".parseUnsafe[Scenario]
  val scenario_33_33 = "[(3 3) (3 3)]".parseUnsafe[Scenario]
  val scenario_22_33 = "[(2 2) (3 3)]".parseUnsafe[Scenario]

  test("Parsing of CHSH lifting to I3322") {
    val text = "[(2 2) (2 2)] -> [(2 2 {0 0}) (2 2 {0 0})]"
    val scenarioA = scenario_CHSH
    val scenarioB = scenario_222_222
    val lifting = text.parseUnsafe[Lifting]
    assert(lifting.toText == text)
    assert(lifting.source.scenario == scenarioA)
    assert(lifting.target.scenario == scenarioB)
  }

  test("Parsing of CHSH lifting to I2233") {
    val text = "[(2 2) (2 2)] -> [({0 0 1} {0 1 1}) ({0 1 1} {0 0 1})]"
    val scenarioA = scenario_CHSH
    val scenarioB = scenario_33_33
    val lifting = text.parseUnsafe[Lifting]
    assert(lifting.toText == text)
    assert(lifting.source.scenario == scenarioA)
    assert(lifting.target.scenario == scenarioB)
  }

  val expr_CHSH = DExpr.parseExpression(scenario_CHSH,
    "P(0,0|0,0) + P(0,0|0,1) + P(0,0|1,0) - P(0,0|1,1) + P(1,1|0,0) + P(1,1|0,1) + P(1,1|1,0) - P(1,1|1,1)"
    ).getOrElse(sys.error("")).projected

  val expr_222_222 = DExpr.parseExpression(scenario_222_222,
    "P(0,0|0,0) + P(0,0|0,1) + P(0,0|1,0) - P(0,0|1,1) + P(1,1|0,0) + P(1,1|0,1) + P(1,1|1,0) - P(1,1|1,1)"
    ).getOrElse(sys.error("")).projected

  val expr_22_33 = DExpr.parseExpression(scenario_22_33,
    "P(0,0|0,0) + P(0,0|0,1) + P(0,0|1,0) - P(0,0|1,1) + P(1,1|0,0) + P(1,1|0,1) + P(1,1|1,0) - P(1,1|1,1) " +
      "+ P(1,2|0,0) + P(1,2|0,1) + P(1,2|1,0) - P(1,2|1,1)").getOrElse(sys.error("")).projected

  test("Example ii) of Pironio doi:10.1063/1.1928727") {
    val lifting = "[(2 2) (2 2)] -> [(2 2 {0 0}) (2 2 {0 0})]".parseUnsafe[Lifting]
    assert((expr_CHSH <|+|? lifting).get == expr_222_222)
  }

  test("Example (3) of Pironio doi:10.1063/1.1928727") {
    val lifting = "[(2 2) (2 2)] -> [(2 2) ({0 1 1} {0 1 1})]".parseUnsafe[Lifting]
    assert((expr_CHSH <|+|? lifting).get == expr_22_33)
  }

  val chshTargets = Table("target",
    "[(2 {0 1 1}) (2 {0 1 1})]",
    "[({0 0 1} {0 1 1}) ({0 1 1} {0 1 1})]",
    "[({0 0 1} {0 1 1} {0 0}) ({0 1 1} {0 1 1} {0 0})]",
    "[(2 2 {0 0}) ({0 1 0 1} 2 {0 0})]",
    "[(2 {0} 2 {0 0}) (2 2 {0 0})]"
  )

  test("Liftings of CHSH") {
    val source = Grouping.noLifting(Scenario.CHSH)
    val chsh = Expr.CHSH
    forAll(chshTargets) { targetText =>
      val target = targetText.parseUnsafe[Grouping]
      val lifting = Lifting(source, target)
      val liftedCHSH = (chsh <|+|? lifting).get
      val unliftedCHSH = (lifting ?|+|> liftedCHSH).get
      chsh should ===(unliftedCHSH)
    }
    forAll(Groupings.genCompatibleGrouping(source)) { target =>
      val lifting = Lifting(source, target)
      val liftedCHSH = (chsh <|+|? lifting).get
      val unliftedCHSH = (lifting ?|+|> liftedCHSH).get
      chsh should ===(unliftedCHSH)
    }
  }

  test("Liftings of I3322") {
    val source = Grouping.noLifting(Scenario.nmk(2,3,2))
    val i3322 = Expr.I3322
    forAll(Groupings.genCompatibleGrouping(source), Groupings.genCompatibleGrouping(source)) { (step1, step2) =>
      val res1 = (i3322 <|+|? Lifting(source, step1)).get
      val res2 = (res1 <|+|? Lifting(step1, step2)).get
      val back = (res2 <|+|? Lifting(step2, source)).get
      i3322 should ===(back)
    }
  }

}
