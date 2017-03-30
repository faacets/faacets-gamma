package com.faacets
package operation
package product
/*
import org.scalacheck._
import org.scalatest.FunSuite

import spire.math.Rational
import spire.implicits._

import qalg.immutable.{QVector, QMatrix}

/** Test suite for `Rank1`
  * 
  * We check that the rank 1 decomposition is performed correctly.
  */
class Rank1Suite extends FunSuite {
  test("Simple matrix") {
    val matrix = QMatrix.rowMajor( (3,3),
      0, 2, 3,
      2, 4, 6,
      3, 6, 9)
    val Some((s, (c, r))) = Rank1.decompositionWithShift(matrix)
    val recMatrix = (c.t.t * r.t) + (Rank1.QMatrixTopLeftOne(c.length, r.length) :* s)
    assert(matrix == recMatrix)
  }
}

object Rank1Spec extends Properties("Rank1") {
  val genNonZeroRational = for {
    num <- Gen.choose(1, 10)
    sign <- Gen.oneOf(-1, 1)
    den <- Gen.choose(1, 5)
  } yield Rational(sign * num)/Rational(den)

  val genRational = for {
    num <- Gen.choose(-10, 10)
    den <- Gen.choose(1, 5)
  } yield Rational(num)/Rational(den)

  val genMostlyZeroRational = for {
    r <- genRational
    b <- Gen.oneOf(0, 1)
  } yield r * b

  val genVec = for {
    n <- Gen.choose(2, 10)
    vec <- Gen.containerOfN[Vector, Rational](n, genMostlyZeroRational)
  } yield QVector(vec:_*)

  val genRank1Matrix = for {
    col <- genVec
    row <- genVec
    factor <- genNonZeroRational
  } yield (col.t.t * row.t) :* factor

  val genRank1MatrixWithShift = for {
    matrix <- genRank1Matrix
    shift <- genRational
  } yield matrix + (Rank1.QMatrixTopLeftOne(matrix.rows, matrix.cols) :* shift)

  property("decomposition") =  Prop.forAll(genRank1Matrix) {
    matrix => {
      val Some((c, r)) = Rank1.decomposition(matrix)
      val recMatrix = (c.t.t * r.t)
      matrix == recMatrix
    }
  }

  property("decompositionWithShift") = Prop.forAll(genRank1MatrixWithShift) {
    matrix => {
      val Some((s, (c, r))) = Rank1.decompositionWithShift(matrix)
      val recMatrix = (c.t.t * r.t) + (Rank1.QMatrixTopLeftOne(c.length, r.length) :* s)
      matrix == recMatrix
    }
  }
}
*/