package com.faacets
package operation
package lifting

import spire.algebra.partial.PartialAction
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.groupoid._
import spire.util.Opt

import com.faacets.core._

final class LiftingExprPartialAction extends PartialAction[Expr, Lifting] {

  def partialActl(l: Lifting, expr: Expr) = partialActr(expr, l.inverse)

  /** Adds or removes single output inputs (those written {0}).
    *
    * For that, the correlators representation is relevant, as those inputs do not
    * appear in the nonsignaling correlator basis.
    */
  protected def liftingSingleOutputInputs(expr: Expr, l: Lifting): Expr = {
    def filterSingleOutputInputs(s: Scenario) = Scenario(s.parties.map(p => Party(p.inputs.filterNot(_ == 1))))
    val sourceScenario = l.source.scenario
    val targetScenario = l.target.scenario
    require(expr.scenario === sourceScenario)
    require(filterSingleOutputInputs(sourceScenario) === filterSingleOutputInputs(targetScenario))
    if (sourceScenario === targetScenario) expr // already good, so no op
    else Expr.correlators(targetScenario, expr.correlators)
  }

  /** Modifies output groupings, without changing the number of inputs. */
  protected def liftingSameNumberOfInputs(expr: Expr, l: Lifting): Expr = {
    require(l.source.compactOutputs.scenario === l.target.compactOutputs.scenario)
    require(expr.scenario === l.source.scenario)
    // we don't verify the grouping (expensive)
    if (l.isId) expr else {
      val n = l.nParties
      val sourceGrouping = l.source
      val targetGrouping = l.target
      val srcA = new Array[Int](n)
      val targetCoeffs = targetGrouping.scenario.tabulateP { (tgtA, arrayX) =>
        cforRange(0 until n) { p =>
          val targetA = tgtA(p)
          val x = arrayX(p)
          val blockInd = targetGrouping.parties(p).inputs(x).partition.blockIndex(targetA)
          srcA(p) = sourceGrouping.parties(p).inputs(x).partition.blocks(blockInd).min
        }
        expr.coefficient(srcA, arrayX)
      }
      // the operation does not produce coefficients in the canonical nonsignaling basis
      DExpr(targetGrouping.scenario, targetCoeffs).projected
    }
  }

  def partialActr(expr: Expr, lifting: Lifting): Opt[Expr] = {
    val exprGrouping = Grouping(expr)
    if (exprGrouping =!= lifting.source) Opt.empty[Expr] else {
      /* The lifting is performed in several steps, using the following primitive operations:
         - modifying the output groupings without changing the number of inputs (in liftingSameNumberOfInputs)
         - adding or removing inputs with a single output (in liftingSingleOutputInputs)
         The first operation is done in the P(ab..|xy..) space, while adding or removing single output inputs is
         done in the (generalized) correlators basis.
         The addition/removal of single output inputs is done between step2 and step3 below, while
         step1->step2 and step3->step4 modify the groupings.
        */
      val step1 = lifting.source
      val step2 = lifting.source.compactOutputs
      val step3 = lifting.target.compactOutputs
      val step4 = lifting.target
      val res2 = liftingSameNumberOfInputs(expr, Lifting(step1, step2))
      val res3 = liftingSingleOutputInputs(res2, Lifting(step2, step3))
      Opt(liftingSameNumberOfInputs(res3, Lifting(step3, step4)))
    }
  }

  override def actrIsDefined(expr: Expr, l: Lifting) = Grouping(expr) === l.source

}
