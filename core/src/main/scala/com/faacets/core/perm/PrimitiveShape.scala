package com.faacets
package core
package perm

import net.alasc.algebra._
import net.alasc.wreath.Divisor
import net.alasc.syntax.all._

final class PrimitiveShape private (val sizes: Array[Int]) {
  @inline def n = sizes.length
  require(n <= 64)
  override def toString = s"PrimitiveShape(${sizes.mkString(", ")})"
  val factors: Array[Int] = sizes.scanLeft(1)(_ * _)
  val size: Int = factors.last
  val fs: Array[Long] = new Array[Long](n)
  val shifts: Array[Byte] = new Array[Byte](n)
  val offsets: Long = {
    var res = 0L
    var i = 0
    var rem = size
    while (i < n) {
      val Divisor(_, max, offset, f, shift) = Divisor(rem - 1, sizes(i))
      assert(rem - 1 <= max)
      offset match {
        case 0 =>
        case 1 => res += (1 << i)
        case _ => sys.error("Unsupported offset")
      }
      fs(i) = f
      shifts(i) = shift.toByte
      rem /= sizes(i)
      i += 1
    }
    res
  }
  def divide(k: Int, idx: Int): Int =
    (((k.toLong + ((offsets & (1 << idx)) >>> idx)) * fs(idx)) >>> shifts(idx)).toInt
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
