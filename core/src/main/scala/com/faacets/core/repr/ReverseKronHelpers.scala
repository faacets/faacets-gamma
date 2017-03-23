package com.faacets
package core
package repr

import scala.annotation.tailrec
import scalin.immutable.{Mat, Vec}
import scalin.immutable.dense._
import spire.math.Rational
import scalin.syntax.all._

/** Helper for matrix-vector products, when the matrix is a Kronecker product
  * 
  * This speeds up dramatically the representation conversion of `Corr`, for example.
  */
object ReverseKronHelpers {

  def revKronMatVec1(matrix: Mat[Rational], dims: Array[Int], dim: Int, v: Vec[Rational]): (Vec[Rational], Array[Int]) = {
    val n = dims.length
    val dC = matrix.nCols
    val dR = matrix.nRows
    require(dims(dim) == dC)
    require(dims.product == v.length)
    val newDims = dims.clone
    newDims(dim) = dR
    val newVec = Vec.tabulate(newDims.product)(newI => {
      val newInds = new Array[Int](n)
      var rem = newI
      var d = 0
      while (d < n) {
        newInds(d) = rem % newDims(d)
        rem /= newDims(d)
        d += 1
      }
      val r = newInds(dim)
      var c = 0
      var newVal = Rational.zero
      while (c < dC) {
        if (matrix(r, c) != Rational.zero) {
          d = 0
          var i = 0
          var factor = 1
          while (d < n) {
            if (d == dim)
              i += c * factor
            else
              i += newInds(d) * factor
            factor *= dims(d)
            d += 1
          }
          newVal += matrix(r, c) * v(i)
        }
        c += 1
      }
      newVal
    })
    (newVec, newDims)
  }
  
  def revKronMatVec(matrices: Seq[Mat[Rational]], origVec: Vec[Rational]): Vec[Rational] = {
    val origDims = matrices.map(_.nCols).toArray
    @tailrec def inner(currentVec: Vec[Rational], currentDims: Array[Int], dim: Int): (Vec[Rational], Array[Int]) = {
      if (dim == matrices.length)
        (currentVec, currentDims)
      else {
        val (newVec, newDims) = revKronMatVec1(matrices(dim), currentDims, dim, currentVec)
        inner(newVec, newDims, dim + 1)
      }
    }
    inner(origVec, origDims, 0)._1
  }

}
