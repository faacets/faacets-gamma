package com.faacets.operation
package product

import com.faacets.core.{Expr, Party, Scenario}
import com.faacets.operation.ProductExtractor
import net.alasc.domains.Partition
import scalin.immutable.{Mat, Vec}
import spire.util.Opt
import scalin.immutable.dense._
import spire.math.Rational
import spire.syntax.cfor._
import scalin.syntax.all._
import instances.expr._

final class ExprProductExtractor extends ProductExtractor[Expr] {

  def cwa = CanonicalWithAffineExtractor.forV[Expr]

  import ProductExtractor.{integerToVector, nonTrivialBipartitions, allBipartitions, sub2ind, ind2sub}

  // def isComposite(e: Expr) = !findPartition(e).isEmpty

  def partialExtract(expr: Expr): Opt[PolyProduct[Expr]] = {
    val scenario = expr.scenario
    val sizes = expr.scenario.parties.map(_.shapeNC.size)
    val cCoefficients = expr.correlators
    allBipartitions(scenario.nParties).foreach { partition =>
      testPartition(expr, partition) match {
        case Opt((shift, (expr0, expr1))) => return Opt(create(partition, shift, expr0, expr1))
        case _ =>
      }
    }
    Opt.empty[PolyProduct[Expr]]
  }

  // tests if the expression can be split around the given partition,
  // and returns the constant factor to be added to the original expression
  // for it to be a genuine product
  def testPartition(expr: Expr, partition: Partition): Opt[(Rational, (Expr, Expr))] = {
    val block0 = partition.blocks(0).toSeq.sorted
    val block1 = partition.blocks(1).toSeq.sorted
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

  protected def create(partition: Partition, shift: Rational, expr1: Expr, expr2: Expr): PolyProduct[Expr] = {
    val (Affine(m1, s1), c1) = CanonicalWithAffineExtractor[Expr].apply(expr1).withoutAffine
    val (Affine(m2, s2), c2) = CanonicalWithAffineExtractor[Expr].apply(expr2).withoutAffine
    PolyProduct.merge2(partition, forceExtract(expr1), forceExtract(expr2), m1 * m2, m1 * s2, m2 * s1, s1 * s2 + shift)
    val coeffs: Map[Set[Int], Rational] = Map(Set(0, 1) -> (m1 * m2), Set(0) -> (m1 * s2), Set(1) -> (m2 * s1), Set.empty -> (s1 * s2 + shift))
    assert(partialExtract(expr1).isEmpty) // TODO support multiple levels of products
    assert(partialExtract(expr2).isEmpty)
    PolyProduct(PartitionPolynomial(partition, coeffs), Vector(c1, c2))
  }

}


/*
  def partialExtract(expr: Expr): Nullbox[ProductShape] = {
    val testExpr = expr.to(testRepresentation(expr.representation))
    val partitions = ProductExtractor.allBipartitions(expr.scenario.nParties)
      .sortBy(_.blocks.map(_.size).min)

    for (partition <- partitions) {
      testPartition(testExpr, partition) match {
        case Nullbox(shift) => return Nullbox(ProductShape(partition, shift))
        case _ =>
      }
    }
    Nullbox.empty[ProductShape]
  }*/
