package com.faacets
package core

import com.faacets.laws.{Parties, PartyRelabelings}
import net.alasc.finite.Grp
import net.alasc.laws.{Grps, Permutations}
import net.alasc.perms.Perm
import net.alasc.perms.default._
import org.scalacheck.{Arbitrary, Gen}
import spire.algebra.Eq
import spire.std.map._
import syntax.subgroups._

case class SmallIndex(toInt: Int)

object SmallIndex {

  implicit def arb: Arbitrary[SmallIndex] = Arbitrary { Gen.choose(0, 4).map(SmallIndex(_)) }

  implicit def toInt(pi: SmallIndex): Int = pi.toInt

}

class PartyRelabelingSubgroupsSuite extends FaacetsSuite {

  test("Output subgroups are correctly split") {
    import spire.std.int._
    import spire.std.map._
    import Grps.arbGrp
    implicit def arbPerm: Arbitrary[Perm] = Arbitrary { Permutations.permForSize(5) }
    forAll { (p1: Grp[Perm], p2: Grp[Perm], i1: SmallIndex, i2: SmallIndex) =>
      def outputPerm(i: Int, p: Perm): PartyRelabeling = PartyRelabeling(Map(i -> p), Perm.id)

      def opGrp(i: Int, g: Grp[Perm]): Grp[PartyRelabeling] =
        Grp.fromGenerators(g.generators.map(outputPerm(i, _)))

      val grp = opGrp(i1, p1) union opGrp(i2, p2)
      val grp1 = Grp.fromGenerators(grp.smallGeneratingSet) // shake generators
      if (i1 == i2)
        grp1.outputPermSubgroups should ===(Map(i1.toInt -> (p1 union p2)).filterNot(_._2.isTrivial))
      else
        grp1.outputPermSubgroups should ===(Map(i1.toInt -> p1, i2.toInt -> p2).filterNot(_._2.isTrivial))
    }
  }

}
class RelabelingSubgroupsSuite extends FaacetsSuite {

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

  test("Party subgroups are correctly split") {
    import spire.std.int._
    import spire.std.map._
    import PartyRelabelings.arbPartyRelabeling
    import Parties.Large._
    import Grps.arbGrp
    forAll { (pr1: Grp[PartyRelabeling], pr2: Grp[PartyRelabeling], p1: SmallIndex, p2: SmallIndex) =>
      def prGrp(p: Int, g: Grp[PartyRelabeling]): Grp[Relabeling] =
        Grp.fromGenerators(g.generators.map(_.forParty(p)))
      val grp = prGrp(p1, pr1) union prGrp(p2, pr2)
      val grp1 = Grp.fromGenerators(grp.smallGeneratingSet) // shake generators
      if (p1 == p2)
        grp1.partyRelabelingSubgroups should === (Map(p1.toInt -> (pr1 union pr2)).filterNot(_._2.isTrivial))
      else
        grp1.partyRelabelingSubgroups should === (Map(p1.toInt -> pr1, p2.toInt -> pr2).filterNot(_._2.isTrivial))
    }
  }

}
