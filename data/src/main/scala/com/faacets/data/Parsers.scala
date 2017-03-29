package com.faacets.data

import fastparse.WhitespaceApi
import fastparse.noApi._
import spire.algebra.Group
import net.alasc.perms.{Cycle, Cycles, Perm}
import net.alasc.syntax.all._
import spire.math.{Rational, SafeLong}

object Parsers {

  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharsWhile(" \n\t".contains(_)).?)
  }

  import White._

  val zero: P[Int] = P( "0".! ).map(x => 0)

  val positiveInt: P[Int] = P( (CharIn('1'to'9') ~~ CharIn('0'to'9').repX).! ).map(_.toInt)

  val nonNegativeInt: P[Int] = zero | positiveInt

  val negativeInt: P[Int] = P( "-" ~ positiveInt ).map(i => -i)

  val int: P[Int] = negativeInt | nonNegativeInt

  val singleCycle: P[Cycles] = P("(" ~ nonNegativeInt.rep(sep=",") ~ ")").map {
    seq => if (seq.isEmpty) Cycles.id else (Cycle(seq: _*): Cycles)
  }

  val cycles1: P[Cycles] = P( singleCycle.rep(1) ).map( Group[Cycles].combineAll(_) )

  val nonIdPerm: P[Perm] = cycles1.map(_.toPerm)

  val perm: P[Perm] = P("id").map(x => Perm.id) | nonIdPerm

  val positiveSafeLong: P[SafeLong] = P( (CharIn('1'to'9') ~~ CharIn('0'to'9').repX).! ).map(str => SafeLong(BigInt(str)))

  val zeroSafeLong: P[SafeLong] = P( "0".! ).map(x => SafeLong.zero)

  val nonNegativeSafeLong: P[SafeLong] = zeroSafeLong | positiveSafeLong

  val negativeSafeLong: P[SafeLong] = P( "-" ~ positiveSafeLong ).map(s => -s)

  val safeLong: P[SafeLong] = negativeSafeLong | nonNegativeSafeLong

  val nonNegativeRationalInteger: P[Rational] = nonNegativeSafeLong.map(Rational(_))

  val nonNegativeRationalFraction: P[Rational] = (nonNegativeSafeLong ~ "/" ~ positiveSafeLong).map {
    case (n, d) => Rational(n, d)
  }

  val positiveRationalInteger: P[Rational] = positiveSafeLong.map(Rational(_))

  val positiveRationalFraction: P[Rational] = (positiveSafeLong ~ "/" ~ positiveSafeLong).map {
    case (n, d) => Rational(n, d)
  }

  val nonNegativeRational: P[Rational] = nonNegativeRationalFraction | nonNegativeRationalInteger

  val positiveRational: P[Rational] = positiveRationalFraction | positiveRationalInteger

  val negativeRational: P[Rational] = ("-" ~ positiveRational).map(r => -r)

  val rational: P[Rational] = negativeRational | nonNegativeRational

}
