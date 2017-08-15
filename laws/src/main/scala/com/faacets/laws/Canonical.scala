package com.faacets
package laws

import spire.math.Rational
import scalin.immutable.Vec
import scalin.immutable.dense._

import org.scalacheck.{Arbitrary, Gen}

import com.faacets.core._
import com.faacets.data.syntax.textable._

case class Canonical[A](value: A)

object Canonical {

  val positivity = Expr(Scenario.nmk(1,1,2), Vec[Rational](-1, 1))
  val i3322 = Expr(Scenario.nmk(2,3,2), Vec[Rational](-5, 3, -5, 3, -4, 2, 3, -1, 3, -1, 4, -2, -5, 3, -5, 3, 2, -4, 3, -1, 3, -1, -2, 4, -4, 4, 2, -2, 0, 0, 2, -2, -4, 4, 0, 0))
  val chsh = Expr(Scenario.nmk(2,2,2), Vec[Rational](-1, 1, -1, 1, 1, -1, 1, -1, -1, 1, 1, -1, 1, -1, -1, 1))
  val cglmp3 = Expr(Scenario.nmk(2,2,3), Vec[Rational](-1, 0, 1, -1, 0, 1, 0, 1, -1, 1, -1, 0, 1, -1, 0, 0, 1, -1, -1, 1, 0, -1, 1, 0, 0, -1, 1, 1, 0, -1, 1, 0, -1, 0, -1, 1))
  val mermin = Expr(Scenario.nmk(3,2,2), Vec[Rational](-1, 1, 0, 0, 1, -1, 0, 0, 0, 0, -1, 1, 0, 0, 1, -1, 1, -1, 0, 0, -1, 1, 0, 0, 0, 0, 1, -1, 0, 0, -1, 1, 0, 0, -1, 1, 0, 0, 1, -1, 1, -1, 0, 0, -1, 1, 0, 0, 0, 0, 1, -1, 0, 0, -1, 1, -1, 1, 0, 0, 1, -1, 0, 0))
  val pironio = Expr("[(3 2) (2 2 2)]".parseUnsafe[Scenario], Vec[Rational](-7, 5, 5, -3, 5, 7, -5, -5, 7, -9, -5, -5, 7, 7, -9, 5, 5, -7, -3, 5, -5, 7, -5, 7, -9, 5, -7, 5, -3, 5))

  def genExpr: Gen[Expr] = Gen.oneOf(positivity, chsh, cglmp3, mermin, pironio)

  def genCanonical: Gen[Canonical[Expr]] = genExpr.map(Canonical(_))

  implicit def arbCanonicalExpr: Arbitrary[Canonical[Expr]] = Arbitrary(genCanonical)

}
