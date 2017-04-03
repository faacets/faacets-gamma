package com.faacets
package operation
package product

import net.alasc.util._
import scalin.immutable.{Mat, MatEngine, Vec, VecEngine}
import spire.algebra.{Eq, Field, Ring}

import scala.annotation.tailrec
import spire.syntax.all._
import scalin.syntax.all._

object Rank1 {

  def topLeftOne[A:MatEngine:Ring](rows: Int, cols: Int): Mat[A] = Mat.fromMutable[A](rows, cols, Ring[A].zero)( res => res(0, 0) := Ring[A].one )

  def firstOne[A:VecEngine:Ring](n: Int): Vec[A] = Vec.fromMutable[A](n, Ring[A].zero)( res => res(0) := Ring[A].one )

  /** Finds a rank-1 decomposition of a QMatrix.
    * 
    * Returns optionally a decomposition in the form of a pair of vectors (c, r) such that:
    * 
    * (c * r.t) = original matrix.
    */
  def decomposition[A:Eq:Field:VecEngine:MatEngine](matrix: Mat[A]): Option[(Vec[A], Vec[A])] = {

    def findNonZeroInside: OptionTuple2NN = {
      cforRange(0 until matrix.nRows) { r =>
        cforRange(0 until matrix.nCols) { c =>
          if (!matrix(r, c).isZero)
            return SomeTuple2NN(r, c)
        }
      }
      NoneTuple2NN
    }

    findNonZeroInside match {
      case OptionTuple2NN(r0, c0) =>
        def cVec(r: Int) = matrix(r, c0)
        def rVec(c: Int) = matrix(r0, c)
        val factor = matrix(r0, c0) / (rVec(c0) * cVec(r0))

        cforRange(0 until matrix.nRows) { r =>
          cforRange(0 until matrix.nCols) { c =>
            if (matrix(r, c) != factor * cVec(r) * rVec(c))
              return None
          }
        }
        Some((
          Vec.tabulate[A](matrix.nRows)(r => cVec(r) * factor),
          Vec.tabulate[A](matrix.nCols)(c => rVec(c))
        ))
      case _ =>
        Some((Vec.zeros(matrix.nRows), Vec.zeros(matrix.nCols)))
    }

  }

  /** Finds a rank-1 decomposition of a QMatrix, with possible shift.
    * 
    * Returns optionally a decomposition in the form of a tuple (s, (c, r)) such that:
    * 
    * (c * r.t) + s * [1 0 ... 0; 0 ... 0 ; ... ; 0 ... 0] = original matrix,
    * and c and r are vectors with integer coefficients.
    */
  def decompositionWithShift[A:Eq:Field:VecEngine:MatEngine](matrix: Mat[A]): Option[(A, (Vec[A], Vec[A]))] = {
    @tailrec def findTopNonZero(c: Int = 1): NNOption =
      if (c == matrix.nCols) NNNone
      else if (!matrix(0, c).isZero) NNSome(c)
      else findTopNonZero(c + 1)
    @tailrec def findLeftNonZero(r: Int = 1): NNOption =
      if (r == matrix.nRows) NNNone
      else if (!matrix(r, 0).isZero) NNSome(r)
      else findLeftNonZero(r + 1)
    val topNonZero: NNOption = findTopNonZero()
    val leftNonZero: NNOption = findLeftNonZero()

    case class ShiftedQMatrix(original: Mat[A], shift: A) extends Mat[A] {
      def nRows = original.nRows
      def nCols = original.nCols
      def apply(r: Int, c: Int) =
        if (r == 0 && c == 0) original(r, c) + shift else original(r, c)
    }

    def withShift(shift: A): Option[(A, (Vec[A], Vec[A]))] = {
      val matrixWithoutShift = ShiftedQMatrix(matrix, -shift)
      decomposition(matrixWithoutShift).map( (shift, _) )
    }

    (leftNonZero, topNonZero) match {
      case (NNOption(r), NNOption(c)) if matrix(r,c).isZero => None
      case (NNOption(r), NNOption(c)) =>
        val mr0 = matrix(r, 0)
        val m0c = matrix(0, c)
        val mrc = matrix(r, c)
        val m00 = (mr0 * m0c) / mrc
        val shift = matrix(0, 0) - m00
        withShift(shift)
      case _ =>
        withShift(matrix(0, 0))
    }
  }

}
