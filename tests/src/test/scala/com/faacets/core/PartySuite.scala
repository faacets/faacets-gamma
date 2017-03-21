package com.faacets
package core

class PartySuite extends FaacetsSuite {

  test("Homogenous") {
    Party.mk(2, 3).isHomogenous shouldBe true
    Party(Seq(3, 2, 2)).isHomogenous shouldBe false
  }


  test("isCanonical") {
    Party.mk(2, 3).isCanonical shouldBe true
    Party(Seq(3, 2, 2)).isCanonical shouldBe true
    Party(Seq(2, 3, 2)).isCanonical shouldBe false
  }

}
