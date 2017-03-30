package com.faacets
package operation
package affine
/*
import scala.util.Random

import spire.algebra.{Group, Eq}
import spire.math.Rational
import spire.syntax.eq._
import spire.syntax.vectorSpace._
import spire.syntax.ring._

import data._
import core._

import affine._

trait AffineParser extends polyta.RationalParserTrait {
  def idmultpart: Parser[Rational] = "x" ^^ {
    str => Rational.one
  }
  def minusmultpart: Parser[Rational] = "-" ~ "x" ^^ {
    els => -Rational.one
  }
  def explicitmultpart: Parser[Rational] = (rational <~ (opt("*") ~ "x"))
  def multpart: Parser[Rational] = explicitmultpart | minusmultpart | idmultpart
  def shiftpart: Parser[Rational] = opt(rationalCoefficientForceSign) ^^ {
    case Some(rat) => rat
    case None => Rational.zero
  }
  def affine: Parser[Affine] = multpart ~ shiftpart ^^ {
    case (a ~ b) => Affine(a, b)
  }
}

final class AffineParsable extends Parsable[Affine] {
  def dumpMultiplierPart(a: Affine): String = {
    if (a.multiplier == 1) ""
    else if (a.multiplier == -1) "-"
    else a.multiplier.toString + " * "
  }
  def dumpShiftPart(a: Affine): String = a.shift.signum match {
    case -1 => " - " + (-a.shift).toString
    case 0 => ""
    case 1 => " + " + a.shift.toString
  }
  def toText(a: Affine): String = dumpMultiplierPart(a) + "x" + dumpShiftPart(a)

  object Parser extends ParserTrait with AffineParser {
    def phrase = affine
  }
}
*/