package com.faacets
package core
package perm

import net.alasc.wreath.Divisor

final class PrimitiveShape private (val sizes: Array[Int]) {
  @inline def n = sizes.length
  require(n <= 64)
  override def toString = s"PrimitiveShape(${sizes.mkString(", ")})"
  val factors: Array[Int] = sizes.scanLeft(1)(_ * _)
  val size: Int = factors.last
  def divide(k: Int, idx: Int): Int = k / sizes(idx)
  def sub2ind(sub: Array[Int]): Int = {
    var i = 0
    var ind = 0
    while (i < n) {
      ind += factors(i) * sub(i)
      i += 1
    }
    ind
  }
  def ind2sub(ind: Int, sub: Array[Int]): Unit = {
    var rem = ind
    var i = 0
    while (i < n) {
      val nextRem = divide(rem, i)
      sub(i) = rem - nextRem * sizes(i)
      rem = nextRem
      i += 1
    }
  }
}

object PrimitiveShape extends UniquenessCache[Seq[Int], PrimitiveShape] {
  protected def keyFromValue(v: PrimitiveShape): Option[Seq[Int]] = Some(v.sizes.toSeq)
  protected def valueFromKey(k: Seq[Int]): PrimitiveShape = new PrimitiveShape(k.toArray)
}
