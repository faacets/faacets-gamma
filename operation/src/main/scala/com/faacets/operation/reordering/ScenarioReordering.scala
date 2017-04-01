package com.faacets
package operation
package reordering


/*
import spire.algebra.NullboxPartialAction
import spire.syntax.group._
import spire.syntax.eq._
import spire.util.Nullbox

import net.alasc.math.Perm

import core._
import core.perm._

final class ScenarioReorderingAction extends NullboxPartialAction[Scenario, Reordering] {
  def partialActl(reordering: Reordering, target: Scenario): Nullbox[Scenario] = partialActr(target, reordering.inverse)
  def partialActr(source: Scenario, reordering: Reordering): Nullbox[Scenario] =
    if (reordering.source === source) Nullbox(reordering.target) else Nullbox.empty[Scenario]
}


/** Reordering a scenario to its canonical form.
  * 
  * The canonical form of a scenario under reordering of inputs and parties is defined such that
  * 
  * - for any party, the number of outputs per input is decreasing.
  * - parties are sorted in decreasing lexicographic order.
  */
final class ScenarioReorderingExtractor extends OperationExtractor[Scenario, Reordering] {
  def action = Reordering.ScenarioAction
  def groupoid = Reordering.Groupoid
  def identity(scenario: Scenario) = Reordering(scenario, scenario)

  def partialExtract(source: Scenario): Nullbox[Reordering] = {
    val target = Reordering.reorderScenario(source)

    if (source =!= target) Nullbox(Reordering(source, target)) else Nullbox.empty[Reordering]
  }
}
*/