package com.faacets
package operation
package laws
/*
import scala.annotation.tailrec

import org.scalacheck._

import spire.math.Rational
import spire.syntax.action._


import net.alasc.laws._

import core.Expr
import core.laws.OperationGenerator

object Affines {
  import Rationals._

  def genAffine: Gen[Affine] = for {
    mult <- genNonZeroRational
    shift <- genRational
  } yield Affine(mult, shift)

  def genPositiveAffine: Gen[Affine] = for {
    mult <- genPositiveRational
    shift <- genRational
  } yield Affine(mult, shift)

  object Positive {
    implicit def arbAffine: Arbitrary[Affine] = Arbitrary(genPositiveAffine)
  }

  object Mixed {
    implicit def arbAffine: Arbitrary[Affine] = Arbitrary(genAffine)
  }

  implicit val affineGenerator: OperationGenerator[Expr, Affine] =
    OperationGenerator[Expr, Affine](expr => genPositiveAffine)

  implicit val affineInstances: Instances[Affine] =
    Instances(Seq(Affine(-1, 1), Affine(1, 0), Affine(1, 1)))

  implicit val affineCloner: Cloner[Affine] =
    Cloner((a: Affine) => Affine(a.multiplier, a.shift))
}
*/