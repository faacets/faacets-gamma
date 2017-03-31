package com.faacets
package operation
package lifting

import spire.math.Rational
import core._
import spire.syntax.groupoid._
import spire.syntax.eq._
import spire.syntax.cfor._
import spire.algebra.partial.PartialAction
import spire.util.Opt

final class LiftingExprAction extends PartialAction[Expr, Lifting] {

  def partialActl(l: Lifting, expr: Expr) = partialActr(expr, l.inverse)

  def partialActr(expr: Expr, lifting: Lifting): Opt[Expr] = {
    val n = lifting.nParties
    val sourceGrouping = lifting.source
    val targetGrouping = lifting.target
    val ncCoeffs = expr.correlators
    val LiftingSplit(inputLifting, outputLifting) = lifting.split
    val interGrouping = inputLifting.target
    val srcK = new Array[Int](n)
    val srcX = new Array[Int](n)
    val interNCCoeffs = interGrouping.scenario.tabulateNC { (kArray, xArray) =>
      var p = 0
      var isZero = false
      cforRange(0 until n) { p =>
        val k = kArray(p)
        val x = xArray(p)
        if (x == -1) {
          srcK(p) = k
          srcX(p) = x
        } else interGrouping.parties(p).compactFromIndex(x) match {
          case Some(compactX) =>
            srcK(p) = k
            srcX(p) = sourceGrouping.parties(p).indexFromCompact(compactX)
          case None =>
            isZero = true
        }
      }
      if (isZero) Rational.zero else ncCoeffs(expr.scenario.sub2indNC(srcK, srcX))
    }
    val interExprInNP = Expr.correlators(interGrouping.scenario, interNCCoeffs)
    val interA = srcK // reuse the k array
    val targetCoeffs = targetGrouping.scenario.tabulateP { (aArray, xArray) =>
      cforRange(0 until n) { p =>
        val targetA = aArray(p)
        val x = xArray(p)
        val blockInd = targetGrouping.parties(p).inputs(x).partition.blockIndex(targetA)
        interA(p) = interGrouping.parties(p).inputs(x).partition.blocks(blockInd).min
      }
      val interX = xArray
      interExprInNP.coefficient(interA, interX)
    }
    Opt(Expr(targetGrouping.scenario, targetCoeffs))
  }

  override def actrIsDefined(expr: Expr, l: Lifting) = Grouping(expr) === l.source

}
