package com.faacets.operation
package product

import com.faacets.core.{Expr, Party, Scenario}
import com.faacets.operation.ProductExtractor
import net.alasc.partitions.Partition
import scalin.immutable.{Mat, Vec}
import spire.util.Opt
import scalin.immutable.dense._
import spire.math.Rational
import spire.syntax.cfor._
import scalin.syntax.all._
import instances.relabeling._

final class ExprProductExtractor extends ProductExtractor[Expr] {

  def nParties(e: Expr) = e.scenario.nParties

  def partialExtract(expr: Expr): Opt[PolyProduct[Expr]] = {
    val scenario = expr.scenario
    val sizes = expr.scenario.parties.map(_.shapeNC.size)
    val cCoefficients = expr.correlators
    SetPartition.nonTrivialBipartitions(Set(0 until scenario.nParties: _*)).foreach { setPartition =>
      val Seq(b0, b1) = setPartition.parts.toSeq
      testPartition(expr, b0, b1) match {
        case Opt((shift, (expr0, expr1))) =>
          val coeffs = Map(Set(b0, b1) -> Rational.one, Set.empty[Set[Int]] -> shift).filterNot(_._2.isZero)
          return Opt(PolyProduct(Map(b0 -> expr0, b1 -> expr1), coeffs))
        case _ =>
      }
    }
    Opt.empty[PolyProduct[Expr]]
  }

  // tests if the expression can be split around the given partition,
  // and returns the constant factor to be added to the original expression
  // for it to be a genuine product
  def testPartition(expr: Expr, part0: Set[Int], part1: Set[Int]): Opt[(Rational, (Expr, Expr))] = {
    val block0 = part0.toSeq.sorted
    val block1 = part1.toSeq.sorted
    val scenario0 = Scenario(block0.map(expr.scenario.parties(_)))
    val scenario1 = Scenario(block1.map(expr.scenario.parties(_)))
    val n = expr.scenario.nParties
    val n0 = block0.size
    val n1 = block1.size
    val inds = new Array[Int](n)
    val inds0 = new Array[Int](n0)
    val inds1 = new Array[Int](n1)
    val shape = expr.scenario.shapeNC
    val shape0 = scenario0.shapeNC
    val shape1 = scenario1.shapeNC
    val cCoefficients = expr.correlators
    // build matrix of coefficients for rank test
    val matrix = Mat.fromMutable[Rational](shape0.size, shape1.size, Rational.zero) { res =>
      cforRange(0 until cCoefficients.length) { ind =>
        shape.ind2sub(ind, inds)
        cforRange(0 until shape0.n)( i => inds0(i) = inds(block0(i)) )
        cforRange(0 until shape1.n)( i => inds1(i) = inds(block1(i)) )
        val ind0 = shape0.sub2ind(inds0)
        val ind1 = shape1.sub2ind(inds1)
        res(ind0, ind1) := cCoefficients(ind)
      }
    }
    Rank1.decompositionWithShift(matrix) match {
      case Some((shift, (coeffs0, coeffs1))) => Opt((shift, (Expr.correlators(scenario0, coeffs0), Expr.correlators(scenario1, coeffs1))))
      case _ => Opt.empty[(Rational, (Expr, Expr))]
    }
  }

}
