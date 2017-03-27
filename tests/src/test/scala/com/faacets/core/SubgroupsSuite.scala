package com.faacets
package core

import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.perms.default._

import spire.std.map._
import spire.std.int._

class SubgroupsSuite extends FaacetsSuite {

  test("CHSH symmetry group decomposition") {
    val symGroup = Expr.CHSH.symmetryGroup
    val subgroups = RelabelingSubgroups(symGroup)
    subgroups.outputsSubgroup should === (Grp(rel"A0(0,1) A1(0,1) B0(0,1) B1(0,1)"))
    subgroups.inputsOutputsSubgroup should === (Grp(
      rel"A0(0,1) A1(0,1) B0(0,1) B1(0,1)",
      rel"A1(0,1) B(0,1)",
      rel"B1(0,1) A(0,1)"))
    subgroups.partiesSubgroup should === (Grp(rel"(A,B)"))
  }

  test("Outputs subgroups of Sliwa 4") {
    RelabelingSubgroups(Expr.Sliwa4.symmetryGroup).outputPermSubgroups should === (Map((0, 1) -> Grp(Perm(0,1))))
  }

}
