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

  implicit val textable: Textable[Reordering] = Textable.fromParser(Parsers.reordering, _.toString)??? //new ReorderingParsable

  /*implicit val Groupoid: NullboxGroupoid[Reordering] = new ReorderingGroupoid
  implicit val ScenarioAction: NullboxPartialAction[Scenario, Reordering] =
    new ScenarioReorderingAction
  implicit val ScenarioReorderingExtractor: OperationExtractor[Scenario, Reordering] =
    new ScenarioReorderingExtractor
  implicit val ExprReorderingAction: NullboxPartialAction[Expr, Reordering] = new VecReorderingAction[Expr]
  implicit val CorrReorderingAction: NullboxPartialAction[Corr, Reordering] = new VecReorderingAction[Corr]
  implicit val ExprReorderingExtractor: OperationExtractor[Expr, Reordering] =
    new VecReorderingExtractor[Expr]
  implicit val CorrReorderingExtractor: OperationExtractor[Corr, Reordering] =
    new VecReorderingExtractor[Corr]*/
}
