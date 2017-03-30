package com.faacets
package operation
package product
/*
import scala.annotation.tailrec
import spire.syntax.all._
import spire.math.Rational

import net.alasc.util._

import qalg.GenQMatrix
import qalg.immutable.{QVector, QMatrix}

object Rank1 {
  def QMatrixTopLeftOne(rows: Int, cols: Int): QMatrix =
    QMatrix.tabulate(rows, cols) {
      (r, c) => if (r == 0 && c == 0) Rational.one else Rational.zero }

  def QVectorFirstOne(n: Int): QVector =
    QVector.tabulate(n)( i => if (i == 0) Rational.one else Rational.zero )


  /** Finds a rank-1 decomposition of a QMatrix.
    * 
    * Returns optionally a decomposition in the form of a pair of vectors (c, r) such that:
    * 
    * (c * r.t) = original matrix.
    */
  def decomposition(matrix: GenQMatrix): Option[(QVector, QVector)] = {

    def findNonZeroInside: OptionTuple2NN = {
      cforRange(0 until matrix.rows) { r =>
        cforRange(0 until matrix.cols) { c =>
          if (matrix(r, c).signum != 0)
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

        cforRange(0 until matrix.rows) { r =>
          cforRange(0 until matrix.cols) { c =>
            if (matrix(r, c) != factor * cVec(r) * rVec(c))
              return None
          }
        }
        Some((
          QVector.tabulate(matrix.rows)(r => cVec(r) * factor),
          QVector.tabulate(matrix.cols)(c => rVec(c))
        ))
      case _ =>
        Some((QVector.zeros(matrix.rows), QVector.zeros(matrix.cols)))
    }
  }

  /** Finds a rank-1 decomposition of a QMatrix, with possible shift.
    * 
    * Returns optionally a decomposition in the form of a tuple (s, (c, r)) such that:
    * 
    * (c * r.t) + s * [1 0 ... 0; 0 ... 0 ; ... ; 0 ... 0] = original matrix,
    * and c and r are vectors with integer coefficients.
    */
  def decompositionWithShift(matrix: GenQMatrix): Option[(Rational, (QVector, QVector))] = {
    @tailrec def findTopNonZero(c: Int = 1): NNOption =
      if (c == matrix.cols) NNNone
      else if (matrix(0, c).signum != 0) NNSome(c)
      else findTopNonZero(c + 1)
    @tailrec def findLeftNonZero(r: Int = 1): NNOption =
      if (r == matrix.rows) NNNone
      else if (matrix(r, 0).signum != 0) NNSome(r)
      else findLeftNonZero(r + 1)
    val topNonZero: NNOption = findTopNonZero()
    val leftNonZero: NNOption = findLeftNonZero()

    case class ShiftedQMatrix(original: GenQMatrix, shift: Rational) extends GenQMatrix {
      def rows = original.rows
      def cols = original.cols
      def apply(ind: Int) =
        if (ind == 0) original(0) + shift else original(ind)
      def toArray: Array[Rational] = Array.tabulate(length)(apply(_))
      def unsafeToArray = toArray
    }

    def withShift(shift: Rational): Option[(Rational, (QVector, QVector))] = {
      val matrixWithoutShift = ShiftedQMatrix(matrix, -shift)
      decomposition(matrixWithoutShift).map( (shift, _) )
    }
    (leftNonZero, topNonZero) match {
      case (NNOption(r), NNOption(c)) if matrix(r,c).signum == 0 => None
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
*/