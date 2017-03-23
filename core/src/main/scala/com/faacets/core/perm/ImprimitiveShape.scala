package com.faacets
package core
package perm

// TODO: when the sizes are homogenous, simply use division

// TODO: rename for better terminology between indices, offsets...
final class ImprimitiveShape private (val sizes: Array[Int]) {

  require(sizes.length <= 128)
  @inline def n = sizes.length

  val offsets: Array[Int] = sizes.scanLeft(0)(_ + _)

  val size = offsets.last

  val blockIndices: Array[Byte] = {
    val res = new Array[Byte](size)
    var i = 0
    var ind = 0
    while (i < n) {
      var j = sizes(i) - 1
      while (j >= 0) {
        res(ind) = i.toByte
        ind += 1
        j -= 1
      }
      i += 1
    }
    res
  }

  def blockOffset(ind: Int) = ind - offsets(blockIndices(ind))

}

object ImprimitiveShape extends UniquenessCache[Seq[Int], ImprimitiveShape] {

  protected def keyFromValue(v: ImprimitiveShape): Option[Seq[Int]] = Some(v.sizes.toSeq)

  protected def valueFromKey(k: Seq[Int]): ImprimitiveShape = new ImprimitiveShape(k.toArray)

}
