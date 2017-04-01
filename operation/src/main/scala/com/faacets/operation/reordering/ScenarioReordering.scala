package com.faacets
package operation
package reordering

import spire.syntax.groupoid._
import spire.syntax.eq._
import core._
import spire.algebra.partial.PartialAction
import spire.util.Opt

final class ScenarioReorderingAction extends PartialAction[Scenario, Reordering] {

  def partialActl(reordering: Reordering, target: Scenario): Opt[Scenario] = partialActr(target, reordering.inverse)
  def partialActr(source: Scenario, reordering: Reordering): Opt[Scenario] =
    if (reordering.source === source) Opt(reordering.target) else Opt.empty[Scenario]
}

/** Reordering a scenario to its canonical form.
  * 
  * The canonical form of a scenario under reordering of inputs and parties is defined such that
  * 
  * - for any party, the number of outputs per input is decreasing.
  * - parties are sorted in decreasing lexicographic order.
  */
final class ScenarioReorderingExtractor extends OperationExtractor[Scenario, Reordering] {

  def action = Reordering.scenarioAction

  def groupoid = Reordering.groupoid

  def identity(scenario: Scenario) = Reordering(scenario, scenario)

  def partialExtract(source: Scenario): Opt[Reordering] = {
    val target = Reordering.reorderScenario(source)

    if (source =!= target) Opt(Reordering(source, target)) else Opt.empty[Reordering]
  }

}
