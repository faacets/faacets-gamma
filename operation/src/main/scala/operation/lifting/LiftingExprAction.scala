package com.faacets
package operation
package lifting
/*

import spire.algebra.NullboxPartialAction
import spire.math.Rational
import spire.syntax.eq._
import spire.syntax.groupoid._
import spire.syntax.cfor._
import spire.syntax.field._
import spire.util.Nullbox

import core._

final class LiftingExprAction extends NullboxPartialAction[Expr, Lifting] {
  def partialActl(l: Lifting, expr: Expr) = partialActr(expr, l.inverse)
  def partialActr(expr: Expr, lifting: Lifting): Nullbox[Expr] = {
    val n = lifting.nParties
    val sourceGrouping = lifting.source
    val targetGrouping = lifting.target

    expr.representation match {
      case WRepresentation | TRepresentation => Nullbox.empty[Expr]
      case SPRepresentation | SCRepresentation | SGRepresentation =>
        val srcExprSP = expr.to(SPRepresentation)
        val liftedInputsForParties: Seq[Set[Int]] = (0 until n).map { p =>
          val inputGrouping = sourceGrouping.parties(p)
          inputGrouping.inputs.indices.filter(inputGrouping.inputs(_).isLiftedInput).toSet
        }
        val srcA = new Array[Int](n)
        val srcX = new Array[Int](n)
        cforRange(0 until srcExprSP.coefficients.length) { ind =>
          if (!srcExprSP.coefficients(ind).isZero) {
            srcExprSP.scenario.ind2subP(ind, srcA, srcX)
            cforRange(0 until n) { p =>
              if (liftedInputsForParties(p).contains(srcX(p))) return Nullbox.empty[Expr]
            }
          }
        }
        val tgtExprSP = Expr.buildSP(targetGrouping.scenario) { (tgtA, tgtX) =>
          var isZero = false
          cforRange(0 until n) { p =>
            val tx = tgtX(p)
            val ta = tgtA(p)
            targetGrouping.parties(p).compactFromIndex(tx) match {
              case None => isZero = true
              case Some(cx) =>
                val ca = targetGrouping.parties(p).inputs(tx).partition.blockIndex(ta)
                val sx = sourceGrouping.parties(p).indexFromCompact(cx)
                val sa = sourceGrouping.parties(p).inputs(sx).partition.blocks(ca).start
                srcX(p) = sx
                srcA(p) = sa
            }
          }
          if (isZero) Rational.zero else srcExprSP.coefficients(srcExprSP.scenario.sub2indP(srcA, srcX))
        }
        Nullbox(tgtExprSP.to(expr.representation))
      case NCRepresentation | NPRepresentation | NGRepresentation =>
        val n = lifting.nParties
        val ncExpr = expr.to(NCRepresentation)
        val LiftingSplit(inputLifting, outputLifting) = lifting.split
        val interGrouping = inputLifting.target
        val srcK = new Array[Int](n)
        val srcX = new Array[Int](n)
        val interExprInNC = Expr.buildNC(interGrouping.scenario) { (kArray, xArray) =>
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
          if (isZero) Rational.zero else ncExpr.coefficientNC(srcK, srcX)
        }
        val interExprInNP = interExprInNC.to(NPRepresentation)
        val interA = srcK // reuse the k array
        val targetExpr = Expr.buildSP(targetGrouping.scenario) { (aArray, xArray) =>
          cforRange(0 until n) { p =>
            val targetA = aArray(p)
            val x = xArray(p)
            val blockInd = targetGrouping.parties(p).inputs(x).partition.blockIndex(targetA)
            interA(p) = interGrouping.parties(p).inputs(x).partition.blocks(blockInd).min
          }
          val interX = xArray
          interExprInNP.coefficientP(interA, interX)
        }
        Nullbox(targetExpr.to(expr.representation))
    }
  }
  override def actrIsDefined(expr: Expr, l: Lifting) = Grouping(expr) === l.source
}
*/