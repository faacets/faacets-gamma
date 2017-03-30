package com.faacets
package operation
/*
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.math.Rational
import spire.syntax.eq._
import spire.syntax.action._

import qalg.immutable.QVector

import net.alasc.math._
import net.alasc.math.enum.Representative
import net.alasc.syntax.subgroup._

import data._
import com.faacets.core._
import com.faacets.core.perm._

import operation.syntax.symmetricForms._
class VecSymmetricFormsCheck(val baseExpr: Expr, val expectedPartiesOrder: BigInt, val symmetricForm: Expr => Representative[Expr, Relabeling]) extends PropSpec with Matchers with EqMatchers with GeneratorDrivenPropertyChecks with NonImplicitAssertions {
  implicit def arbitrary: Arbitrary[Relabeling] = Arbitrary {
    Gen.parameterized(params => baseExpr.scenario.group.randomElement(params.rng))
  }
  property("(expr <|+| rl) has a symmetric form") {
    forAll { (rl: Relabeling) =>
      symmetricForm(baseExpr <|+| rl).get.symmetryGroup.partiesSubgroup.order shouldBe expectedPartiesOrder
    }
  }
}

class VecSymmetricFormsI3322 extends VecSymmetricFormsCheck(Expr.I3322, 2, _.symmetricRepresentative)

class VecSymmetricFormsSliwa4 extends VecSymmetricFormsCheck(Expr.Sliwa4, 2, _.symmetricRepresentative)

class VecSymmetricFormsSliwa7 extends VecSymmetricFormsCheck(Expr.Sliwa7, 6, _.symmetricRepresentative)

class VecSymmetricFormsSliwa10 extends VecSymmetricFormsCheck(Expr.Sliwa10, 3, _.cyclicRepresentative)

*/