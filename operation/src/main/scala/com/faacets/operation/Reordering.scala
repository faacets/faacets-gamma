package com.faacets
package operation

import spire.syntax.eq._
import data._
import core._
import reordering._
import spire.algebra.Eq
import spire.algebra.partial.{Groupoid, PartialAction}

case class Reordering(source: Scenario, target: Scenario) {

  import Reordering._

  require(reorderScenario(source) === reorderScenario(target))

  override def toString = s"$source -> $target"

}

object Reordering {

  implicit val lexicographicPartyOrdering: Ordering[Party] = LexicographicPartyOrder.toOrdering

  def reorderParty(party: Party) =
    Party(party.inputs.sorted.reverse)

  def reorderScenario(scenario: Scenario) =
    Scenario(scenario.parties.map(reorderParty).sorted.reverse)

  implicit val equ: Eq[Reordering] = Eq.fromUniversalEquals[Reordering]

  implicit val textable: Textable[Reordering] = Textable.fromParser[Reordering](Parsers.reordering, _.toString)

  implicit val groupoid: Groupoid[Reordering] = new ReorderingGroupoid

  implicit val scenarioAction: PartialAction[Scenario, Reordering] = new ScenarioReorderingAction
  implicit val scenarioReorderingExtractor: OperationExtractor[Scenario, Reordering] = new ScenarioReorderingExtractor
  implicit val exprReorderingAction: PartialAction[Expr, Reordering] = new VecReorderingPartialAction[Expr]
  implicit val dExprReorderingAction: PartialAction[DExpr, Reordering] = new VecReorderingPartialAction[DExpr]
  implicit val behaviorReorderingAction: PartialAction[Behavior, Reordering] = new VecReorderingPartialAction[Behavior]
  implicit val exprReorderingExtractor: OperationExtractor[Expr, Reordering] = new VecReorderingExtractor[Expr]
  implicit val dExprReorderingExtractor: OperationExtractor[DExpr, Reordering] = new VecReorderingExtractor[DExpr]
  implicit val behaviorReorderingExtractor: OperationExtractor[Behavior, Reordering] = new VecReorderingExtractor[Behavior]

}
