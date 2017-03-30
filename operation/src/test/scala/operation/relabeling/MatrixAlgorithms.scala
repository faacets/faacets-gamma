package com.faacets
package operation
package relabeling
/*
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.algebra.Order
import spire.syntax.eq._
import spire.syntax.order._
import spire.syntax.action._
import spire.std.int._

import net.alasc.algebra._
import net.alasc.math.{Grp, Perm}
import net.alasc.std.seq._
import net.alasc.syntax.subgroup._

class MatrixAlgorithmsSuite extends FunSuite {
  test("Simple case") {
    val matrix = Seq(Seq(4, 3, 2, 1), Seq(4, 3, 2, 1), Seq(4, 3, 2, 1), Seq(4, 3, 2, 1))
    val nrows = 4
    val ncols = 4
    val rGrp = Grp(Perm(0,1), Perm(0,1,2,3))
    val cGrp = Grp(Perm(0,1), Perm(0,1,2,3))
    val rRep = Perm.Representations.R(nrows)
    val cRep = Perm.Representations.R(ncols)
    val (permR, permC) = MatrixAlgorithms.findMinimalPermutation(nrows, ncols, (r, c) => matrix(r)(c), rGrp, cGrp, rRep, cRep)
    val matrixMin = Seq.tabulate[Int](nrows, ncols)( (r,c) => matrix(r <|+| permR)(c <|+| permC) )
    val matrixMinShouldBe = Seq(Seq(1,2,3,4), Seq(1,2,3,4), Seq(1,2,3,4), Seq(1,2,3,4))
    assert(matrixMin == matrixMinShouldBe)
  }
}

object MatrixAlgorithmsCheck extends Properties("MatrixAlgorithms") {
  def genPerm(n: Int): Gen[Perm] = for {
    seq <- Gen.containerOfN[Seq, Int](n, Gen.choose(1, 10000))
  } yield Perm.Algebra.sorting(seq)

  def genSeq(n: Int): Gen[Seq[Int]] = Gen.containerOfN[Seq, Int](n, Gen.choose(1, 3))

  case class Case(nrows: Int, ncols: Int, matrix: Seq[Seq[Int]], rGrp: Grp[Perm], cGrp: Grp[Perm])

  def genCase = for {
    nrows <- Gen.choose(3, 10)
    ncols <- Gen.choose(3, 10)
    matrix <- Gen.containerOfN[Seq, Seq[Int]](nrows, genSeq(ncols))
    nRGens <- Gen.choose(1, 3)
    nCGens <- Gen.choose(1, 3)
    rGens <- Gen.containerOfN[Seq, Perm](nRGens, genPerm(nrows))
    cGens <- Gen.containerOfN[Seq, Perm](nCGens, genPerm(ncols))
    rGrp = Grp.fromGenerators(rGens)
    cGrp = Grp.fromGenerators(cGens)
  } yield Case(nrows, ncols, matrix, rGrp, cGrp)

  property("minimum is unique") = Prop.forAllNoShrink(genCase) {
    case Case(nrows, ncols, matrix, rGrp, cGrp) =>
      implicit def permAction: FaithfulPermutationAction[Perm] = Perm.Algebra
      val randR = rGrp.randomElement(scala.util.Random)
      val randC = cGrp.randomElement(scala.util.Random)
      val rRep = Perm.Representations.R(nrows)
      val cRep = Perm.Representations.R(ncols)

      val matrix1 = Seq.tabulate[Int](nrows, ncols)( (r,c) => matrix(r <|+| randR)(c <|+| randC) )

      val (permR, permC) = MatrixAlgorithms.findMinimalPermutation(nrows, ncols, (r, c) => matrix(r)(c), rGrp, cGrp, rRep, cRep)
      val (permR1, permC1) = MatrixAlgorithms.findMinimalPermutation(nrows, ncols, (r, c) => matrix1(r)(c), rGrp, cGrp, rRep, cRep)

      val res = Seq.tabulate[Int](nrows, ncols)( (r, c) => matrix(r <|+| permR)(c <|+| permC) )
      val res1 = Seq.tabulate[Int](nrows, ncols)( (r, c) => matrix1(r <|+| permR1)(c <|+| permC1) )
      res == res1
  }
}
*/