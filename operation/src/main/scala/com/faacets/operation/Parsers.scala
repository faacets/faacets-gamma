package com.faacets.operation

import com.faacets.operation.lifting.GroupingParsers
import GroupingParsers.grouping
import fastparse.noApi._
import spire.math.Rational

object Parsers {

  import com.faacets.data.Parsers.{rational, White, nonNegativeRational}
  import com.faacets.core.Parsers.scenario

  import White._

  import GroupingParsers.grouping

  val lifting: P[Lifting] = P( grouping ~ "->" ~ grouping ).flatMap {
    case (source, target) => Lifting.validate(source, target).fold(s => Fail.opaque(s.toList.mkString(",")), l => Pass.map(x => l))
  }

  val reordering: P[Reordering] = P( scenario ~ "->" ~ scenario ).map { case (s, t) => Reordering(s, t) }

  def idmultpart: P[Rational] = P("x").map(str => Rational.one)

  def minusmultpart: P[Rational] = P("-" ~ "x").map(str => -Rational.one)

  def explicitmultpart: P[Rational] = P(rational ~ ("*".? ~ "x"))

  def multpart: P[Rational] = P(explicitmultpart | minusmultpart | idmultpart)

  def sign: P[Rational] = P("+").map(x => Rational.one) | P("-").map(x => -Rational.one)

  def rationalCoefficientForceSign: P[Rational] = P(sign ~ nonNegativeRational).map { case (s, r) => s * r }

  def shiftpart: P[Rational] = rationalCoefficientForceSign.?.map {
    case Some(rat) => rat
    case None => Rational.zero
  }

  def affine: P[Affine] = P(multpart ~ shiftpart).map {
    case (a, b) => Affine(a, b)
  }

}
