package com.faacets
package operation
package relabeling
/*
import org.scalatest._
import org.scalacheck._
import prop._

import spire.algebra._
import spire.math.Rational
import spire.syntax.all._

import qalg.immutable.QVector

import core._
import perm.Relabeling

import operation.laws._
import core.laws._

class RepresentativesCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {
  import Prop._

  implicit def arbExpr = Arbitrary(Canonical.genExpr)

  property("Looking for minimal form is stable") {
    forAll { (expr: Expr) =>
      val cexpr = expr.representatives.head.get
      val cexpr1 = cexpr.representatives.head.get
      cexpr shouldBe cexpr1
    }
  }

  property("Symmetry group of the expr is rightly translated by permutedBy") {
    forAll { (expr: Expr) =>
      forAll(Relabelings.genRelabeling(expr)) { r =>
        val exprPermuted = expr <|+| r
        val exprPermuted1 = Expr(expr.scenario, expr.representation, exprPermuted.coefficients)
        exprPermuted.symmetryGroup shouldBe exprPermuted1.symmetryGroup
      }
    }
  }

  property("Check minimal lexicographic ordering using Partition") {
    forAll { (expr: Expr) =>
      whenever(expr.scenario.nParties > 1) {
        forAll(Relabelings.genRelabeling(expr)) { r =>
          val minExpr = expr.representatives.apply(BigInt(0)).get
          val expr1 = expr <|+| r
          val expr1Perm = Partitions.findMinimal(expr1)
          val minExpr1 = expr1 <|+| expr1Perm
          minExpr shouldBe minExpr1
        }
      }
    }
  }
}
*/