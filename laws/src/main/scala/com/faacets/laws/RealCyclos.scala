package com.faacets.laws

import cyclo.RealCyclo
import org.scalacheck.{Arbitrary, Gen}
import spire.math.Rational

object RealCyclos {

  def genSmallPositiveRational: Gen[Rational] = for {
    num <- Gen.choose(1, 4)
    den <- Gen.choose(1, 5)
  } yield Rational(num, den)


  def genRealCycloFromRational: Gen[RealCyclo] = Rationals.genRational.map(RealCyclo(_))

  def genRealCycloFromSqrt: Gen[RealCyclo] = genSmallPositiveRational.map(RealCyclo.sqrt(_))

  def genRealCycloFromCos: Gen[RealCyclo] = genSmallPositiveRational.map(RealCyclo.cosRev(_))

  def genRealCycloFromSin: Gen[RealCyclo] = genSmallPositiveRational.map(RealCyclo.sinRev(_))

  def genBaseRealCyclo: Gen[RealCyclo] = Gen.oneOf(
    genRealCycloFromRational,
    genRealCycloFromSqrt,
    genRealCycloFromCos,
    genRealCycloFromSin
  )

  def genRealCyclo: Gen[RealCyclo] = Gen.oneOf(
    genBaseRealCyclo.flatMap(a => genBaseRealCyclo.map(b => a + b)),
    genBaseRealCyclo.flatMap(a => genBaseRealCyclo.map(b => a - b)),
    genBaseRealCyclo.flatMap(a => genBaseRealCyclo.map(b => a * b))
  )

  implicit def arbRealCyclo: Arbitrary[RealCyclo] = Arbitrary(genRealCyclo)

}
