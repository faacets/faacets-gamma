package com.faacets
package core

import com.faacets.laws.DataLaws
import net.alasc.laws.AnyRefLaws

class PartySuite extends FaacetsSuite {

  import com.faacets.laws.Parties.Huge._
  import com.faacets.laws.Parties.{partyCloner, partyInstances}
  checkAll("Party", DataLaws[Party].textable)
  checkAll("Party", AnyRefLaws[Party]._eq)

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
