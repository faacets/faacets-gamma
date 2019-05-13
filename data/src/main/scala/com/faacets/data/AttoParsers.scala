package com.faacets.data

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

import cats.implicits._
import spire.math.interval.{Bound, Closed, Unbound}
import spire.math.{Interval, Point, Rational}

import cyclo.RealCyclo

import net.alasc.syntax.permutationAction._
import atto._
import Atto._
import net.alasc.perms.{Cycle, Cycles, Perm}
import syntax.attoParser._

// Parser convention: inside the parser, we consume whitespace, but not to the left or right of the thing parsed

/** Numeric parsers */
object NumericParsers {

  val zeroBigInt: Parser[BigInt] = char('0') >| BigInt(0)

  val positiveBigInt: Parser[BigInt] =
    (charRange('1' to '9'), takeWhile(c => 0 <= c && c <= 9)).mapN((x,y) => BigInt(x +: y))

  val nonNegativeBigInt: Parser[BigInt] = zeroBigInt | positiveBigInt

  val negativeBigInt: Parser[BigInt] = ( string("-") ~> positiveBigInt ).map(i => -i)

  val bigInt: Parser[BigInt] = ( negativeBigInt | nonNegativeBigInt ).namedOpaque("big-int")

  val nonNegativeInt: Parser[Int] = nonNegativeBigInt.filter(_.isValidInt).map(_.toInt).namedOpaque("nonnegative-int")
  val positiveInt: Parser[Int] = positiveBigInt.filter(_.isValidInt).map(_.toInt).namedOpaque("positive-int")
  val int: Parser[Int] = bigInt.filter(_.isValidInt).map(_.toInt).namedOpaque("int")

  val signAsInt: Parser[Int] = (char('+') >| 1) | (char('-') >| -1)

  val nonNegativeRationalFraction: Parser[Rational] =
    (token(nonNegativeBigInt) <~ token(char('/')), positiveBigInt).mapN(Rational(_, _))

  val positiveRationalFraction: Parser[Rational] =
    (token(positiveBigInt) <~ token(char('/')), positiveBigInt).mapN(Rational(_, _))

  val nonNegativeRational: Parser[Rational] =
    (nonNegativeRationalFraction | nonNegativeBigInt.map(Rational(_))).namedOpaque("nonnegative-rational")

  val positiveRational: Parser[Rational] =
    (positiveRationalFraction | positiveBigInt.map(Rational(_))).namedOpaque("positive-rational")

  val negativeRational: Parser[Rational] =
    (token(char('-')) ~> positiveRational).map(r => -r)

  val rational: Parser[Rational] = (negativeRational | nonNegativeRational).namedOpaque("rational")

}

object RealCycloParsers {
  import NumericParsers._

  val signlessPiFraction: Parser[Rational] =
    (opt(token(positiveBigInt) <~ token(char('*'))), token(string("pi")).void, opt(token(char('/')) ~> positiveBigInt))
        .mapN( (num, _, den) => Rational(num.getOrElse(BigInt(1)), den.getOrElse(BigInt(1))) )

  val negativePiFraction: Parser[Rational] = (token(char('-')) ~> signlessPiFraction).map(r => -r)

  val piFraction: Parser[Rational] = negativePiFraction | signlessPiFraction
  val cosPi: Parser[RealCyclo] = (token(string("cos")) ~> parens(piFraction)).map(r => RealCyclo.cosRev(r / 2))
  val sinPi: Parser[RealCyclo] = (token(string("sin")) ~> parens(piFraction)).map(r => RealCyclo.sinRev(r / 2))
  val sqrt: Parser[RealCyclo] = (token(string("sqrt")) ~> parens(nonNegativeRational)).map(RealCyclo.sqrt)
  val functions: Parser[RealCyclo] = cosPi | sinPi | sqrt

  val dividedFunctions: Parser[RealCyclo] = (token(functions), token(char('/')) ~> positiveBigInt).mapN(_ / _)

  val signlessFactorFunctions: Parser[RealCyclo] =
    (opt(token(nonNegativeRational) <~ token(char('*'))), dividedFunctions | functions)
      .mapN( (factor, value) => value * factor.getOrElse(Rational.one) )

  val signlessTerm: Parser[RealCyclo] = signlessFactorFunctions | nonNegativeRational.map(RealCyclo(_))

  val realCycloHeadTerm: Parser[RealCyclo] =
    (opt(signAsInt).map(_.getOrElse(1)), signlessTerm).mapN( (sign, value) => value * sign )

  val realCycloTailTerm: Parser[RealCyclo] = (signAsInt, signlessTerm).mapN( (sign, value) => value * sign )

  val realCycloComposite: Parser[RealCyclo] = (token(realCycloHeadTerm), realCycloTailTerm.sepBy1(skipWhitespace))
    .mapN( (hd, tl) => tl.foldLeft(hd)(_ + _) )

  val realCyclo: Parser[RealCyclo] = realCycloComposite | realCycloHeadTerm
}

object PermParsers {

  import NumericParsers._
  import MySyntax._

  val emptyCycles: Parser[Cycles] = parens(ok(Cycles.id))
  val singleCycle: Parser[Cycle] = parens(token(nonNegativeInt).sepBy(token(char(','))))
    .check(_.size)(_ > 1, (_, n) => s"Cycles need to apply to at least 2 elements, but here $n were provided")
    .map(Cycle(_: _*))

  @tailrec def hasOverlap[A](listOfLists: List[Iterable[A]], acc: HashSet[A] = HashSet.empty[A]): Boolean =
    listOfLists match {
      case Nil => false
      case hd :: tl =>
        val newAcc = acc ++ hd
        if (newAcc.size < acc.size + hd.size) true else hasOverlap(tl, newAcc)
    }

  val nonEmptyCycles: Parser[Cycles] = singleCycle.sepBy1(skipWhitespace)
    .map(_.toList)
    .check(_.map(_.seq))(!hasOverlap[Int](_), (_, cycles) => s"Given cycles should be disjoint, which fails for $cycles")
    .map(cycles => Cycles.fromDisjointCycles(cycles))

  val nonIdPerm: Parser[Perm] = nonEmptyCycles.map(_.toPerm)

  val idPerm: Parser[Perm] = string("id") >| Perm.id

  val perm: Parser[Perm] = (idPerm | nonIdPerm).namedOpaque("permutation")

}

object ScalarParsers {

  import NumericParsers._
  import RealCycloParsers.realCyclo

  val digits: Parser[String] = takeWhile(c => '0' <= c && c <= '9')
  val zeroDigits: Parser[String] = string("0")
  val nonZeroDigits: Parser[String] = (charRange('1' to '9'), digits)
    .mapN(_ +: _)

  val leadingDigits: Parser[String] = zeroDigits | nonZeroDigits
  val signlessBigDecimal: Parser[Scalar] = (leadingDigits, string("."), digits)
    .mapN(_ ++ _ ++ _).map(string => Scalar.Decimal(Rational.one, BigDecimal(string), Rational.zero))

  val signlessDividedBigDecimal: Parser[Scalar] = (token(signlessBigDecimal), token(char('/')) ~> positiveBigInt).mapN(_ / _)
  val signlessFactorBigDecimal: Parser[Scalar] = (opt(token(nonNegativeRational) <~ token(char('*'))), signlessDividedBigDecimal | signlessBigDecimal)
    .mapN( (factor, value) => value * factor.getOrElse(Rational.one) )
  val signlessShiftedFactorBigDecimal: Parser[Scalar] = (token(signlessFactorBigDecimal), token(signAsInt), nonNegativeRational)
    .mapN( (bd, sign, shift) => bd + shift * sign )

  val decimalScalar: Parser[Scalar] = (opt(token(signAsInt)), signlessShiftedFactorBigDecimal | signlessFactorBigDecimal)
      .mapN( (signOpt, value) => value * signOpt.getOrElse[Int](1) )
  val exactScalar: Parser[Scalar] = realCyclo.map(Scalar.Exact(_))
  val scalar: Parser[Scalar] = decimalScalar | exactScalar

}

object IntervalScalarParsers {
  import ScalarParsers._

  val point: Parser[Interval[Scalar]] = exactScalar.map(Point(_))
  val lowerClosedBound: Parser[Bound[Scalar]] = (token(char('[')) ~> scalar).map(Closed(_))
  val upperClosedBound: Parser[Bound[Scalar]] = (token(scalar) <~ char(']')).map(Closed(_))
  val lowerUnbound: Parser[Bound[Scalar]] = (token(char(']')) ~ token(char('-')) ~ string("inf")) >| Unbound[Scalar]
  val upperUnbound: Parser[Bound[Scalar]] = (opt(token(char('+'))) ~ token(string("inf")) ~ char('[')) >| Unbound[Scalar]
  val lowerBound: Parser[Bound[Scalar]] = lowerUnbound | lowerClosedBound
  val upperBound: Parser[Bound[Scalar]] = upperUnbound | upperClosedBound
  val bracketIntervalScalar: Parser[Interval[Scalar]] = (token(lowerBound) <~ token(char(',')), upperBound).mapN(Interval.fromBounds(_, _))
  val intervalScalar: Parser[Interval[Scalar]] = bracketIntervalScalar | point
}