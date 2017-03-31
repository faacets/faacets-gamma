package com.faacets
package operation

import spire.syntax.all._
import spire.math.Rational

import core._
import data._

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

  /*

  val expr_CHSH = Expr.parseExpression(scenario_CHSH)(
    "P(0,0|0,0) + P(0,0|0,1) + P(0,0|1,0) - P(0,0|1,1) + P(1,1|0,0) + P(1,1|0,1) + P(1,1|1,0) - P(1,1|1,1)"
    )
  val expr_222_222 = Expr.parseExpression(scenario_222_222)(
    "P(0,0|0,0) + P(0,0|0,1) + P(0,0|1,0) - P(0,0|1,1) + P(1,1|0,0) + P(1,1|0,1) + P(1,1|1,0) - P(1,1|1,1)"
    )
  val expr_22_33 = Expr.parseExpression(scenario_22_33)(
    "P(0,0|0,0) + P(0,0|0,1) + P(0,0|1,0) - P(0,0|1,1) + P(1,1|0,0) + P(1,1|0,1) + P(1,1|1,0) - P(1,1|1,1) " +
      "+ P(1,2|0,0) + P(1,2|0,1) + P(1,2|1,0) - P(1,2|1,1)")

  test("Example ii) of Pironio doi:10.1063/1.1928727") {
    val lifting = "[(2 2) (2 2)] -> [(2 2 {0 0}) (2 2 {0 0})]".fromText[Lifting]
    assert((expr_CHSH <|+| lifting) == expr_222_222)
  }

  test("Example (3) of Pironio doi:10.1063/1.1928727") {
    val lifting = "[(2 2) (2 2)] -> [(2 2) ({0 1 1} {0 1 1})]".fromText[Lifting]
    assert((expr_CHSH <|+| lifting) == expr_22_33)
  }

  def testGroupingInScenario(scenario: Scenario) = {
    for ((ncExpr, _) <- LocalPolytope(scenario).facets) {
      val lifted = ncExpr.to(NPRepresentation)
      val delifting = lifted.forceExtract[Lifting]
      val unlifted = ncExpr <|+| delifting
      val relifted = delifting |+|> unlifted
      assert(ncExpr == relifted)
    }
  }

  test("Unlifting/lifting for inequalities of [(3 3) (2 2 2)]") {
    testGroupingInScenario("[(3 3) (2 2 2)]".fromText[Scenario])
  }

  test("Unlifting/lifting for inequalities of [(5 2) (2 2)]") {
    testGroupingInScenario("[(5 2) (2 2)]".fromText[Scenario])
  }

  val liftingTexts = List("[(2 2) (2 2)] -> [(2 {0 1 1}) (2 {0 1 1})]",
    "[(2 2) (2 2)] -> [({0 0 1} {0 1 1}) ({0 1 1} {0 1 1})]",
    "[(2 2) (2 2)] -> [({0 0 1} {0 1 1} {0 0}) ({0 1 1} {0 1 1} {0 0})]",
    "[(2 2) (2 2)] -> [(2 2 {0 0}) (2 2 {0 0})]"
  )

  test("Lifting split") {
    for (liftingText <- liftingTexts) {
      val lifting = liftingText.fromText[Lifting]
      val LiftingSplit(inputLifting, outputLifting) = lifting.split
      assert(lifting.source.scenario == inputLifting.source.scenario)
      assert(inputLifting.target.scenario == outputLifting.source.scenario)
      assert(outputLifting.target.scenario == lifting.target.scenario)
    }
  }

  test("Liftings of CHSH") {
    for (liftingText <- liftingTexts) {
      val lifting = liftingText.fromText[Lifting]
      val liftedCHSH = chsh <|+| lifting
      val unliftedCHSH = lifting |+|> liftedCHSH
      assert(chsh == unliftedCHSH)
    }
  }
*/
}
