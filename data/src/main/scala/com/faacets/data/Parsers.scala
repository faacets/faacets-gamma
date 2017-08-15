package com.faacets.data

import spire.algebra.Group
import spire.math.interval.{Bound, Closed, Unbound}
import spire.math.{Interval, Point, Rational, SafeLong}
import cyclo.RealCyclo
import net.alasc.perms.{Cycle, Cycles, Perm}
import net.alasc.syntax.all._

import fastparse.WhitespaceApi
import fastparse.noApi._

object Parsers {

  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharsWhile(" \n\t".contains(_)).?)
  }

  import White._

  val zero: P[Int] = P( "0".! ).map(x => 0)

  val positiveInt: P[Int] = P( (CharIn('1'to'9') ~~ CharIn('0'to'9').repX).! ).map(_.toInt)

  val nonNegativeInt: P[Int] = P(zero | positiveInt).opaque("<nonneg-integer>")

  val negativeInt: P[Int] = P( "-" ~ positiveInt ).map(i => -i)

  val int: P[Int] = P( negativeInt | nonNegativeInt ).opaque("<integer>")

  val singleCycle: P[Cycles] = P("(" ~ nonNegativeInt.rep(sep=",") ~ ")").opaque("<cycle>").map {
    seq => if (seq.isEmpty) Cycles.id else (Cycle(seq: _*): Cycles)
  }

  val cycles1: P[Cycles] = P( singleCycle.rep(1) ).map( Group[Cycles].combineAll(_) )

  val nonIdPerm: P[Perm] = cycles1.map(_.toPerm)

  val perm: P[Perm] = P( P("id").map(x => Perm.id) | nonIdPerm ).opaque("<permutation>")

  val positiveSafeLong: P[SafeLong] = P( (CharIn('1'to'9') ~~ CharIn('0'to'9').repX).! ).map(str => SafeLong(BigInt(str)))

  val zeroSafeLong: P[SafeLong] = P( "0".! ).map(x => SafeLong.zero)

  val nonNegativeSafeLong: P[SafeLong] = P( zeroSafeLong | positiveSafeLong ).opaque("<nonneg-integer>")

  val negativeSafeLong: P[SafeLong] = P( "-" ~ positiveSafeLong ).map(s => -s)

  val safeLong: P[SafeLong] = P( negativeSafeLong | nonNegativeSafeLong ).opaque("<integer>")

  val nonNegativeRationalInteger: P[Rational] = nonNegativeSafeLong.map(Rational(_))

  val nonNegativeRationalFraction: P[Rational] = (nonNegativeSafeLong ~ "/" ~ positiveSafeLong).map {
    case (n, d) => Rational(n, d)
  }

  val positiveRationalInteger: P[Rational] = positiveSafeLong.map(Rational(_))

  val positiveRationalFraction: P[Rational] = (positiveSafeLong ~ "/" ~ positiveSafeLong).map {
    case (n, d) => Rational(n, d)
  }

  val nonNegativeRational: P[Rational] = P( nonNegativeRationalFraction | nonNegativeRationalInteger ).opaque("<nonneg-rational>")

  val positiveRational: P[Rational] = positiveRationalFraction | positiveRationalInteger

  val negativeRational: P[Rational] = ("-" ~ positiveRational).map(r => -r)

  val rational: P[Rational] = P(negativeRational | nonNegativeRational).opaque("<rational>")

  val signAsRational: P[Rational] = P("+").map(x => Rational.one) | P("-").map(x => -Rational.one)

  object realCyclo {
    val signlessPiFraction: P[Rational] = P( (positiveSafeLong ~ "*").? ~ "pi" ~ ("/" ~ positiveSafeLong).? ).map {
      case (n, d) => Rational(n.getOrElse(SafeLong.one), d.getOrElse(SafeLong.one))
    }
    val negativePiFraction: P[Rational] = P("-" ~ signlessPiFraction).map(r => -r)
    val piFraction: P[Rational] = P( negativePiFraction | signlessPiFraction )
    val cosPi: P[RealCyclo] = P( "cos(" ~ piFraction ~ ")" ).map(r => RealCyclo.cosRev(r/2))
    val sinPi: P[RealCyclo] = P( "sin(" ~ piFraction ~ ")" ).map(r => RealCyclo.sinRev(r/2))
    val sqrt: P[RealCyclo] = P( "sqrt(" ~ nonNegativeRational ~ ")" ).map(r => RealCyclo.sqrt(r) )
    val derived: P[RealCyclo] = P( cosPi | sinPi | sqrt )
    val signlessRational: P[RealCyclo] = P( nonNegativeRational ).map(r => RealCyclo(r))
    val signlessRationalDerived: P[RealCyclo] = P( (nonNegativeRational ~ "*").? ~ derived ~ ("/" ~ positiveSafeLong).?).map {
      case (r, rc, d) => rc * r.getOrElse(Rational.one)/d.getOrElse(SafeLong.one)
    }
    val signlessTerm: P[RealCyclo] = P( signlessRationalDerived | signlessRational )
    val realCycloFirstTerm: P[RealCyclo] = P(signAsRational.? ~ signlessTerm).map {
      case (s, rc) => rc * s.getOrElse(Rational.one)
    }
    val realCycloNextTerm: P[RealCyclo] = P(signAsRational ~ signlessTerm).map {
      case (s, rc) => rc * s
    }
    val realCyclo: P[RealCyclo] = P( realCycloFirstTerm ~ realCycloNextTerm.rep ).map {
      case (first, next) => next.fold(first)(_ + _)
    }
  }

  object scalar {
    val zeroDigits = P( "0" )
    val nonZeroDigits = P(CharIn('1'to'9') ~~ CharIn('0'to'9').repX)
    val digits = P(zeroDigits | nonZeroDigits)
    val bigDecimal: P[BigDecimal] = P( ( digits ~~ "." ~~ CharIn('0'to'9').repX ).! ).map(bd => BigDecimal(bd) )
    val signlessScaledBigDecimal: P[Scalar] = P( (nonNegativeRational ~ "*").? ~ bigDecimal ~ ("/" ~ positiveSafeLong).?).map {
      case (r, bd, d) => Scalar.Decimal(r.getOrElse(Rational.one)/Rational(d.getOrElse(SafeLong.one)), bd, Rational.zero)
    }
    val decimalScalar: P[Scalar] = P( signAsRational.? ~ signlessScaledBigDecimal ~ (signAsRational ~ nonNegativeRational).? ).map {
      case (s, sbd, Some((sr, nnr))) => sbd * s.getOrElse(Rational.one) + sr * nnr
      case (s, sbd, None) => sbd * s.getOrElse(Rational.one)
    }
    val exactScalar: P[Scalar] = realCyclo.realCyclo.map(Scalar.Exact(_))
    val scalar: P[Scalar] = P( decimalScalar | exactScalar )
  }

  object intervalScalar {
    val point: P[Point[Scalar]] = P( scalar.exactScalar ).map(s => Point(s))
    val lowerClosedBound: P[Bound[Scalar]] = P( "[" ~ scalar.scalar ).map(x => Closed(x))
    val upperClosedBound: P[Bound[Scalar]] = P( scalar.scalar ~ "]" ).map(x => Closed(x))
    val lowerUnbound: P[Bound[Scalar]] = P("]" ~ "-" ~ "inf").map(x => Unbound[Scalar])
    val upperUnbound: P[Bound[Scalar]] = P("+".? ~ "inf" ~ "[").map(x => Unbound[Scalar])
    val lowerBound: P[Bound[Scalar]] = P(lowerUnbound | lowerClosedBound)
    val upperBound: P[Bound[Scalar]] = P(upperUnbound | upperClosedBound)
    val bracketIntervalScalar: P[Interval[Scalar]] = P( lowerBound ~ "," ~ upperBound ).map {
      case (l, u) => Interval.fromBounds(l, u)
    }
    val intervalScalar: P[Interval[Scalar]] = P( bracketIntervalScalar | point )
  }

}
