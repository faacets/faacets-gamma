package com.faacets
package operation

import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.eq._
import data._
import core._
import core.perm._
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
  implicit val exprReorderingAction: PartialAction[Expr, Reordering] = new VecReorderingAction[Expr]
  implicit val corrReorderingAction: PartialAction[Behavior, Reordering] = new VecReorderingAction[Behavior]
  implicit val exprReorderingExtractor: OperationExtractor[Expr, Reordering] = new VecReorderingExtractor[Expr]
  implicit val corrReorderingExtractor: OperationExtractor[Behavior, Reordering] = new VecReorderingExtractor[Behavior]

}
