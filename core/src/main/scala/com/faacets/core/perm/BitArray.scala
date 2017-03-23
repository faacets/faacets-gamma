package com.faacets
package core
package perm

class BitArray(val long: Long) extends AnyVal {
  import BitArray._
  override def toString = toBooleanSeq(maxSize).map {
    case false => "0"
    case true => "1"}.mkString
  @inline def bool(i: Int): Boolean = (long & bitMask(i)) != 0
  @inline def apply(i: Int): Int = ((long & bitMask(i)) >>> i).toInt
  @inline def updated(i: Int, b: Boolean): BitArray =
    updated(i, booleanToZeroOrOne(b))
  @inline def updated(i: Int, zeroOrOne: Int): BitArray =
    new BitArray((long & holeMask(i)) | (zeroOrOne.toLong << i))
  @inline def orBit(i: Int, zeroOrOne: Int): BitArray =
    new BitArray(long | (zeroOrOne.toLong << i))
  @inline def zeroSlice(start: Int, size: Int): BitArray =
    new BitArray(long & ~BitArray.sliceMask(start, size))
  @inline def updateSlice(start: Int, size: Int, replaceBy: BitArray): BitArray =
    new BitArray((long & ~BitArray.sliceMask(start, size)) | (replaceBy.long << start))
  @inline def set(i: Int): BitArray = new BitArray((long & holeMask(i)) + bitMask(i))
  @inline def unset(i: Int): BitArray = new BitArray(long & holeMask(i))
  @inline def slice(start: Int, n: Int): BitArray = new BitArray((long & rightFillMask(start + n)) >>> start)
  @inline def <<(n: Int): BitArray = new BitArray(long << n)
  @inline def >>(n: Int): BitArray = new BitArray(long >>> n)
  def toBooleanSeq(n: Int): Seq[Boolean] = Seq.tabulate[Boolean](n)(bool)
  @inline def ===(that: BitArray): Boolean = (this.long == that.long)
  @inline def ^(that: BitArray): BitArray = new BitArray(long ^ that.long)
  @inline def &(that: BitArray): BitArray = new BitArray(long & that.long)
  @inline def |(that: BitArray): BitArray = new BitArray(long | that.long)
  @inline def invert(n: Int): BitArray = new BitArray(long ^ rightFillMask(n))
  @inline def isZero: Boolean = (long == 0L)
}

object BitArray {
  @inline def zero: BitArray = new BitArray(0L)
  @inline def ones(n: Int): BitArray = new BitArray( (1L << n) - 1 )
  val maxSize = 64
  def apply(booleanSeq: Seq[Boolean]): BitArray = {
    require(booleanSeq.length <= maxSize)
    (BitArray.zero /: booleanSeq.indices) {
      case (ba, i) if booleanSeq(i) => ba.set(i)
      case (ba, _) => ba
    }
  }
  def apply(bitString: String): BitArray = {
    require(bitString.length <= maxSize)
    new BitArray(BigInt(bitString.reverse, 2).toLong)
  }
  @inline def holeMask(i: Int): Long = ~(1L << i)
  @inline def bitMask(i: Int): Long = (1L << i)
  @inline def rightFillMask(n: Int): Long = ((1L << n) - 1) - ((n & 64) >> 6)
  @inline def sliceMask(start: Int, size: Int): Long = {
    val mask1 = rightFillMask(start + size)
    val mask2 = rightFillMask(start)
    mask1 - mask2
  }
  @inline def rightFill(n: Int): BitArray = new BitArray(rightFillMask(n))
  @inline def slice(start: Int, size: Int) = new BitArray(sliceMask(start, size))
  @inline def booleanToZeroOrOne(b: Boolean): Int = (if (b) 1 else 0)
}
