package com.faacets.data

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import spire.algebra.Order
import spire.math.interval.{Closed, Unbound}
import spire.math._
import cyclo.RealCyclo

import com.faacets.consolidate.{Merge, Path, Result}

case class Value(interval: Interval[Scalar]) {
  def toRealCycloInterval: Interval[RealCyclo] = interval.mapBounds(_.toRealCyclo)
  require {
    interval match {
      case Point(s) => !s.isDecimal
      case Empty() => false
      case b: Bounded[Scalar] => b.lowerBound.isClosed && b.upperBound.isClosed
      case a: Above[Scalar] => a.lowerBound.isClosed
      case b: Below[Scalar] => b.upperBound.isClosed
      case All() => true
    }
  }

  override def toString = interval match {
    case All() => "]-inf, inf["
    case Above(lower, flags) => s"[$lower, inf["
    case Below(upper, flags) => s"]-inf, $upper]"
    case Bounded(lower, upper, flags) => s"[$lower, $upper]"
    case Point(s) => s.toString
    case Empty() => sys.error("Impossible case")
  }
  def *(rhs: Rational): Value =
    if (rhs.isZero) Value(Point(Scalar.Exact(RealCyclo.zero): Scalar)) else Value(interval.mapBounds(_ * rhs))
  def /(rhs: Rational): Value = Value(interval.mapBounds(_ / rhs))
  def +(rhs: Rational): Value = Value(interval.mapBounds(_ + rhs))
  def -(rhs: Rational): Value = Value(interval.mapBounds(_ - rhs))

  def opposite: Value = Value(interval.mapBounds(_.opposite))

  def isExactZero: Boolean = interval match {
    case Point(Scalar.Exact(rc)) => rc.isZero
    case _ => false
  }
}

object Value {

  def apply(s: Scalar.Exact): Value = Value(Point(s: Scalar))
  def apply(rc: RealCyclo): Value = apply(Scalar.Exact(rc))
  def apply(i: Int): Value = apply(RealCyclo(i))
  def apply(r: Rational): Value = apply(RealCyclo(r))

  def validate(interval: Interval[Scalar]): ValidatedNel[String, Value] = {
    def valid = Validated.Valid(Value(interval))
    interval match {
      case All() => valid
      case Empty() => Validated.invalidNel("Intervals cannot be empty, check lower bound < upper bound")
      case Point(s) if s.isDecimal => Validated.invalidNel("Decimal numbers can only enter intervals with lower bound < upper bound")
      case Point(s) => valid
      // remains Above/Below/Bounded
      case a: Above[Scalar] if a.lowerBound.isClosed => valid
      case b: Below[Scalar] if b.upperBound.isClosed => valid
      case b: Bounded[Scalar] if b.lowerBound.isClosed && b.upperBound.isClosed => valid
      case _ => Validated.invalidNel("Open bounds are only supported on the values +/- inf")
    }
  }

  implicit val valueMerge: Merge[Value] = new Merge[Value] {
    def merge(base: Value, other: Value): Result[Value] = {
      val baseI = base.toRealCycloInterval
      val otherI = other.toRealCycloInterval
      if (otherI.isSupersetOf(baseI)) Result.same(base)
      else if (!baseI.intersects(otherI)) Result.failed(NonEmptyList.of(Path.empty -> s"Values $base and $other do not overlap"))
      else {
        if (baseI.isSupersetOf(otherI)) Result.updated(other, NonEmptyList.of(Path.empty -> s"Refined value $base into $other"))
        def extractLB[A](i: Interval[A]): Option[A] = i match {
          case Point(s) => Some(s)
          case Above(s, _) => Some(s)
          case Bounded(s, _, _) => Some(s)
          case _ => None
        }
        def extractUB[A](i: Interval[A]): Option[A] = i match {
          case Point(s) => Some(s)
          case Below(s, _) => Some(s)
          case Bounded(_, s, _) => Some(s)
          case _ => None
        }
        val lbs: Seq[Scalar] = extractLB(base.interval).toSeq ++ extractLB(other.interval).toSeq
        val ubs: Seq[Scalar] = extractUB(base.interval).toSeq ++ extractUB(other.interval).toSeq
        implicit val rcOrdering: Ordering[RealCyclo] = Order[RealCyclo].toOrdering
        val lb = if (lbs.isEmpty) Unbound[Scalar] else Closed(lbs.maxBy(_.toRealCyclo))
        val ub = if (ubs.isEmpty) Unbound[Scalar] else Closed(ubs.minBy(_.toRealCyclo))
        println(lb -> ub)
        Value.validate(Interval.fromBounds(lb, ub)) match {
          case Validated.Valid(newValue) =>
            Result.updated(newValue, NonEmptyList.of(Path.empty -> s"Refined values $base and $other into $newValue"))
          case Validated.Invalid(errors) => Result.failed(errors.map(Path.empty -> _))
        }
      }
    }
  }

  implicit val valueTextable: Textable[Value] =
    Textable.fromAttoParser(Parsers.intervalScalar)
    Textable.fromParserAndValidation(Parsers.intervalScalar.intervalScalar, Value.validate,_.toString)

}


/** Scalar value written either using a member of the real cyclotomic field, or a
  * scaled and shifted decimal approximation.
  */
trait Scalar {
  def isDecimal: Boolean
  def toRealCyclo: RealCyclo
  def *(rhs: Rational): Scalar
  def /(rhs: Rational): Scalar
  def +(rhs: Rational): Scalar
  def -(rhs: Rational): Scalar
  def opposite: Scalar
}

object Scalar {

  implicit val scalarOrder: Order[Scalar] = Order.by[Scalar, RealCyclo](_.toRealCyclo)

  def apply(bd: BigDecimal): Scalar = Decimal(1, bd, 0)
  def apply(r: Rational): Scalar = Exact(RealCyclo(r))
  def apply(l: Long): Scalar = Exact(RealCyclo(l))
  def apply(bi: BigInt): Scalar = Exact(RealCyclo(bi))
  def apply(s: SafeLong): Scalar = Exact(RealCyclo(s))
  def apply(r: RealCyclo): Scalar = Exact(r)

  /** Represents a value of the form "factor * decimal + shift". */
  case class Decimal(factor: Rational, decimal: BigDecimal, shift: Rational) extends Scalar {
    def isDecimal = true
    require(decimal.signum > 0)
    override def toString = {
      val sb = new StringBuilder
      val nnFactor = if (factor < 0) {
        sb ++= "-"
        -factor
      } else factor
      if (!nnFactor.isOne) {
        sb ++= nnFactor.toString
        sb ++= "*"
      }
      sb ++= decimal.toString
      if (!shift.isZero) {
        if (shift.signum > 0)
          sb ++= "+"
        sb ++= shift.toString
      }
      sb.result()
    }
    lazy val toRealCyclo = RealCyclo(Rational(decimal))
    def *(rhs: Rational): Scalar =
      if (rhs.isZero) Exact(RealCyclo.zero) else copy(factor = factor*rhs, shift = shift*rhs)
    def /(rhs: Rational): Scalar = copy(factor = factor/rhs, shift = shift/rhs)
    def +(rhs: Rational): Scalar = copy(shift = shift + rhs)
    def -(rhs: Rational): Scalar = copy(shift = shift - rhs)
    def opposite: Scalar = Decimal(-factor, decimal, -shift)
  }

  /** Represents a value with an exact real cyclotomic number. */
  case class Exact(cyclo: RealCyclo) extends Scalar {
    def isDecimal = false
    override def toString = cyclo.toString
    def toRealCyclo = cyclo
    def *(rhs: Rational): Scalar = Exact(cyclo * RealCyclo(rhs))
    def /(rhs: Rational): Scalar = Exact(cyclo / RealCyclo(rhs))
    def +(rhs: Rational): Scalar = Exact(cyclo + RealCyclo(rhs))
    def -(rhs: Rational): Scalar = Exact(cyclo - RealCyclo(rhs))
    def opposite: Scalar = Exact(-cyclo)
  }

}
