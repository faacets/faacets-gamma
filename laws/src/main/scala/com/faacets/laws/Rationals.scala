package com.faacets.laws

import spire.math.Rational

import org.scalacheck.Gen

object Rationals {

  def genPositiveRational: Gen[Rational] = for {
    num <- Gen.choose(1, 10)
    den <- Gen.choose(1, 10)
  } yield Rational(num, den)

  def genNonZeroRational: Gen[Rational] = for {
    num <- Gen.choose(-10, 10) if num != 0
    den <- Gen.choose(1, 10)
  } yield Rational(num, den)

  def genNonNegativeRational: Gen[Rational] = for {
    num <- Gen.choose(0, 10)
    den <- Gen.choose(1, 10)
  } yield Rational(num, den)

  def genRational: Gen[Rational] = for {
    num <- Gen.choose(-10, 10)
    den <- Gen.choose(1, 10)
  } yield Rational(num, den)

}
