package com.faacets
package core.perm

import spire.laws.GroupLaws

import laws._

import net.alasc.laws.AnyRefLaws

class PartyRelabelingSuite extends FaacetsSuite {

  import Parties.Large._
  import PartyRelabelings._

  checkAll("PartyRelabeling", AnyRefLaws[PartyRelabeling]._eq)
  checkAll("PartyRelabeling", DataLaws[PartyRelabeling].textable)
  checkAll("PartyRelabeling", GroupLaws[PartyRelabeling].group)

}
