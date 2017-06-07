package com.faacets.laws

import cyclo.RealCyclo
import org.scalacheck.{Arbitrary, Gen}

object RealCyclos {

  def genRealCycloFromRational: Gen[RealCyclo] = Rationals.genRational.map(RealCyclo(_))

  def genRealCycloFromSqrt: Gen[RealCyclo] = Rationals.genPositiveRational.map(RealCyclo.sqrt(_))

  def genRealCycloFromCos: Gen[RealCyclo] = Rationals.genRational.map(RealCyclo.cosRev(_))

  def genRealCycloFromSin: Gen[RealCyclo] = Rationals.genRational.map(RealCyclo.sinRev(_))

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
