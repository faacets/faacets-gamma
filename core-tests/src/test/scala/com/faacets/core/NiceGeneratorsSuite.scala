package com.faacets
package core

import net.alasc.finite.Grp
import net.alasc.laws.Grps
import net.alasc.perms.default._
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2

class NiceGeneratorsSuite extends FaacetsSuite {

  val exprs: TableFor2[String, Expr[_ <: Scenario with Singleton]] = Table(
    ("name", "value"),
    ("I3322", Expr.I3322),
    ("Sliwa4", Expr.Sliwa4),
    ("Sliwa7", Expr.Sliwa7),
    ("Sliwa10", Expr.Sliwa10)
  )

  implicit def genRelabeling(expr: Expr[_ <: Scenario with Singleton]): Gen[Relabeling] =
    Grps.genRandomElement(expr.scenario.group)

  test("Grp.fromGenerators(expr.symmetryGroup.conjBy(rl).niceGenerators) has correct order") {
    forAll(exprs) { (name: String, expr: Expr[_ <: Scenario with Singleton]) =>
      val sm: Grp[Relabeling] = expr.symmetryGroup
      forAll(genRelabeling(expr)) { r =>
        Grp.fromGenerators(RelabelingSubgroups(sm.conjugatedBy(r)).niceGenerators.toIndexedSeq).order shouldBe sm.order
      }
    }
  }

  test("expr.symmetryGroup.conjBy(rl).inputsOutputsSubgroup.order is invariant") {
    forAll(exprs) { (name: String, expr: Expr[_ <: Scenario with Singleton]) =>
      val sm: Grp[Relabeling] = expr.symmetryGroup
      forAll(genRelabeling(expr)) { r =>
        RelabelingSubgroups(sm.conjugatedBy(r)).inputsOutputsSubgroup.order should ===(RelabelingSubgroups(sm).inputsOutputsSubgroup.order)
      }
    }
  }

}
