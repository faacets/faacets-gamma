package com.faacets
package operation
package product
/*
import spire.math.Rational
import spire.syntax.all._
import spire.util.Nullbox

import net.alasc.math.Domain

import qalg.immutable.QVector


import core._
import affine._

final class ExprProductExtractor extends ProductExtractor[Expr] {
  def semigroup = ExprSemigroup

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
  }

  def testRepresentation(source: Representation): Representation = source match {
    case NCRepresentation | NPRepresentation | NGRepresentation => NCRepresentation
    case SCRepresentation | SPRepresentation | SGRepresentation => SCRepresentation
    case WRepresentation | TRepresentation => TRepresentation
  }

  // tests if the expression can be split around the given partition,
  // and returns the constant factor to be added to the original expression
  // for it to be a genuine product
  def testPartition(expr: Expr, partition: Domain#Partition): Nullbox[Rational] = {
    val repr = expr.representation
    require(repr == NCRepresentation || repr == SCRepresentation || repr == TRepresentation)
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
    val shape = expr.scenario.primitiveShape(repr)
    val shape0 = scenario0.primitiveShape(repr)
    val shape1 = scenario1.primitiveShape(repr)
    // build matrix of coefficients for rank test
    val matrix = qalg.mutable.QMatrix.zeros(shape0.size, shape1.size)

    cforRange(0 until expr.coefficients.length) { ind =>
      shape.ind2sub(ind, inds)
      cforRange(0 until shape0.n)( i => inds0(i) = inds(block0(i)) )
      cforRange(0 until shape1.n)( i => inds1(i) = inds(block1(i)) )
      val ind0 = shape0.sub2ind(inds0)
      val ind1 = shape1.sub2ind(inds1)
      matrix(ind0, ind1) = expr.coefficients(ind)
    }

    Rank1.decompositionWithShift(matrix.toImmutable) match {
      case Some((shift, _)) => Nullbox(-shift)
      case _ => Nullbox.empty[Rational]
    }
  }
}

*/