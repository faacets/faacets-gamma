package com.faacets
package core.perm

class BitArraySuite extends FaacetsSuite {

  test("Bit string parsing") {
    assert(BitArray("0" * 54 + "1" * 10).toBooleanSeq(64) == (Seq.fill(54)(false) ++ Seq.fill(10)(true)))
  }

  test("Slices") {
    assert(BitArray("111000").slice(3, 3) === BitArray("000"))
    assert(BitArray("0" * 53 + "1" * 10).slice(53, 10) === BitArray("1" * 10))
    assert(BitArray("0" * 54 + "1" * 10).slice(54, 10) === BitArray("1" * 10))
  }

  test("BitArray.slice") {
    assert(BitArray.slice(10, 10).toBooleanSeq(30) ==
      Seq.fill(10)(false) ++ Seq.fill(10)(true) ++ Seq.fill(10)(false))
    assert(BitArray.slice(0, 64) === BitArray.rightFill(64))
  }

}
