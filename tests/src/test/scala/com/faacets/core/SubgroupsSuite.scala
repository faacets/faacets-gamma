package com.faacets
package core

import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.perms.default._

import spire.std.map._
import syntax.subgroups._

class SubgroupsSuite extends FaacetsSuite {

  test("CHSH symmetry group decomposition") {
    val symGroup = Expr.CHSH.symmetryGroup
    symGroup.outputsSubgroup should === (Grp(rel"A0(0,1) A1(0,1) B0(0,1) B1(0,1)"))
    symGroup.inputsOutputsSubgroup should === (Grp(
      rel"A0(0,1) A1(0,1) B0(0,1) B1(0,1)",
      rel"A1(0,1) B(0,1)",
      rel"B1(0,1) A(0,1)"))
    symGroup.partiesSubgroup should === (Grp(rel"(A,B)"))
  }

  test("Outputs subgroups of Sliwa 4") {
    Expr.Sliwa4.symmetryGroup.outputPermSubgroups should === (Map((0, 1) -> Grp(Perm(0,1))))
  }

  test("Output subgroups of zero expression") {
    val pg = Grp(Perm(0,1))
    Expr.zero(Scenario.CHSH).symmetryGroup.outputPermSubgroups should === (Map(
      (0,0) -> pg,
      (1,0) -> pg,
      (0,1) -> pg,
      (1,1) -> pg
    ))
  }

}
