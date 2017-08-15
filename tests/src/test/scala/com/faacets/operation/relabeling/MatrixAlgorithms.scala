package com.faacets
package operation
package relabeling

import spire.std.int._
import net.alasc.algebra._
import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.perms.default._

import org.scalacheck._

class MatrixAlgorithmsSuite extends FaacetsSuite {

  test("Simple case") {
    val matrix = Seq(Seq(4, 3, 2, 1), Seq(4, 3, 2, 1), Seq(4, 3, 2, 1), Seq(4, 3, 2, 1))
    val nrows = 4
    val ncols = 4
    val rGrp = Grp(Perm(0, 1), Perm(0, 1, 2, 3))
    val cGrp = Grp(Perm(0, 1), Perm(0, 1, 2, 3))
    val (permR, permC) = MatrixAlgorithmsMaybeBroken.findMinimalPermutation(nrows, ncols, (r, c) => matrix(r)(c), rGrp, cGrp, Perm.algebra, Perm.algebra)
    val matrixMin = Seq.tabulate[Int](nrows, ncols)((r, c) => matrix(r <|+| permR)(c <|+| permC))
    val matrixMinShouldBe = Seq(Seq(1, 2, 3, 4), Seq(1, 2, 3, 4), Seq(1, 2, 3, 4), Seq(1, 2, 3, 4))
    assert(matrixMin == matrixMinShouldBe)
  }

  def genPerm(n: Int): Gen[Perm] = for {
    seq <- Gen.containerOfN[Seq, Int](n, Gen.choose(1, 10000))
  } yield Perm.sorting(seq)

  def genIndexedSeq(n: Int): Gen[IndexedSeq[Int]] = Gen.containerOfN[IndexedSeq, Int](n, Gen.choose(1, 3))

  case class Case(nrows: Int, ncols: Int, matrix: Seq[Seq[Int]], rGrp: Grp[Perm], cGrp: Grp[Perm])

  def genCase = for {
    nrows <- Gen.choose(3, 10)
    ncols <- Gen.choose(3, 10)
    matrix <- Gen.containerOfN[IndexedSeq, IndexedSeq[Int]](nrows, genIndexedSeq(ncols))
    nRGens <- Gen.choose(1, 3)
    nCGens <- Gen.choose(1, 3)
    rGens <- Gen.containerOfN[IndexedSeq, Perm](nRGens, genPerm(nrows))
    cGens <- Gen.containerOfN[IndexedSeq, Perm](nCGens, genPerm(ncols))
    rGrp = Grp.fromGenerators[Perm](rGens.filterNot(_.isId))
    cGrp = Grp.fromGenerators[Perm](cGens.filterNot(_.isId))
  } yield Case(nrows, ncols, matrix, rGrp, cGrp)

  test("Minimum is unique") {
    forAll(genCase) {
      case Case(nrows, ncols, matrix, rGrp, cGrp) =>
        implicit def permAction: PermutationAction[Perm] = Perm.algebra

        val randR = rGrp.randomElement(scala.util.Random)
        val randC = cGrp.randomElement(scala.util.Random)

        val matrix1 = Seq.tabulate[Int](nrows, ncols)((r, c) => matrix(r <|+| randR)(c <|+| randC))

        val (permR, permC) = MatrixAlgorithmsMaybeBroken.findMinimalPermutation(nrows, ncols, (r, c) => matrix(r)(c), rGrp, cGrp, permAction, permAction)
        val (permR1, permC1) = MatrixAlgorithmsMaybeBroken.findMinimalPermutation(nrows, ncols, (r, c) => matrix1(r)(c), rGrp, cGrp, permAction, permAction)

        val res = Seq.tabulate[Int](nrows, ncols)((r, c) => matrix(r <|+| permR)(c <|+| permC))
        val res1 = Seq.tabulate[Int](nrows, ncols)((r, c) => matrix1(r <|+| permR1)(c <|+| permC1))
        res == res1
    }
  }
}
