package com.faacets.data

import cats.data.NonEmptyList
import cyclo.RealCyclo
import spire.ClassTag
import spire.algebra.Order
import spire.math.{Above, All, Below, Bounded, Empty, Interval, Point, Rational, SafeLong}
import spire.math.interval.{Closed, Unbound}
import com.faacets.consolidate.{Merge, Path, Result}
import spire.syntax.eq._


/** Knowledge about a quantity, either as an exact value or an interval. */
trait Value {
  def toInterval: Interval[RealCyclo]
}

object Value {

  case class Exact(scalar: Scalar) extends Value {
    override def toString = scalar.toString
    def toInterval = scalar.toInterval
  }

  case class Range(interval: Interval[Scalar]) extends Value {
    require(!interval.isEmpty)
    require(!interval.isPoint)
    override def toString = interval match {
      case All() => "]-inf, inf["
      case Above(lower, flags) => s"[$lower, inf["
      case Below(upper, flags) => s"]-inf, $upper]"
      case Bounded(lower, upper, flags) => s"[$lower, $upper]"
      case _: Point[Scalar] | _: Empty[Scalar] => sys.error("Impossible case")
    }
    def toInterval = Interval.fromBounds(
      interval.lowerBound.map(_.lowerBound),
      interval.upperBound.map(_.upperBound)
    )
  }

  implicit val valueMerge: Merge[Value] = new Merge[Value] {
    def merge(base: Value, other: Value): Result[Value] = {
      val baseI = base.toInterval
      val otherI = other.toInterval
      if (otherI.isSupersetOf(baseI)) Result.same(base)
      else if (!baseI.intersects(otherI)) Result.failed(NonEmptyList.of(Path.empty -> s"Values $base and $other do not overlap"))
      else {
        if (baseI.isSupersetOf(otherI)) Result.updated(other, NonEmptyList.of(Path.empty -> s"Refined value $base into $other"))
        def extractLB(v: Value): Option[Scalar] = v match {
          case Exact(s) => Some(s)
          case Range(Above(s, _)) => Some(s)
          case Range(Bounded(s, _, _)) => Some(s)
          case _ => None
        }
        def extractUB(v: Value): Option[Scalar] = v match {
          case Exact(s) => Some(s)
          case Range(Below(s, _)) => Some(s)
          case Range(Bounded(_, s, _)) => Some(s)
          case _ => None
        }
        val lbs: Seq[Scalar] = extractLB(base).toSeq ++ extractLB(other).toSeq
        val ubs: Seq[Scalar] = extractUB(base).toSeq ++ extractUB(other).toSeq
        implicit val rcOrdering: Ordering[RealCyclo] = Order[RealCyclo].toOrdering
        val lb = if (lbs.isEmpty) Unbound[Scalar] else Closed(lbs.maxBy(_.lowerBound))
        val ub = if (ubs.isEmpty) Unbound[Scalar] else Closed(ubs.minBy(_.upperBound))
        println(lb -> ub)
        val newValue = Value.Range(Interval.fromBounds(lb, ub))
        Result.updated(newValue, NonEmptyList.of(Path.empty -> s"Refined values $base and $other into $newValue"))
      }
    }
  }

  implicit val valueTextable: Textable[Value] = Textable.fromParser(Parsers.value.value, _.toString)

}

/** Scalar value written either using a member of the real cyclotomic field, or a
  * scaled and shifted decimal approximation.
  */
trait Scalar {
  def lowerBound: RealCyclo
  def midPoint: RealCyclo
  def upperBound: RealCyclo
  def toInterval: Interval[RealCyclo]
  def *(rhs: Rational): Scalar
  def /(rhs: Rational): Scalar
  def +(rhs: Rational): Scalar
  def -(rhs: Rational): Scalar
}

object Scalar {

  implicit val scalarOrder: Order[Scalar] = Order[RealCyclo].on[Scalar](_.midPoint)

  def apply(bd: BigDecimal): Scalar = Decimal(1, bd, 0)
  def apply(r: Rational): Scalar = Exact(RealCyclo(r))
  def apply(l: Long): Scalar = Exact(RealCyclo(l))
  def apply(bi: BigInt): Scalar = Exact(RealCyclo(bi))
  def apply(s: SafeLong): Scalar = Exact(RealCyclo(s))
  def apply(r: RealCyclo): Scalar = Exact(r)

  /** Represents a value of the form "factor * decimal + shift". */
  case class Decimal(factor: Rational, decimal: BigDecimal, shift: Rational) extends Scalar {
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
    val asRational = Rational(decimal)
    val halfWidth = Rational(BigDecimal(5, decimal.scale + 1))
    def lowerBound = RealCyclo((asRational - halfWidth)*factor + shift)
    def midPoint = RealCyclo(asRational)
    def upperBound = RealCyclo((asRational + halfWidth)*factor + shift)
    def toInterval = Interval.closed(lowerBound, upperBound)
    def *(rhs: Rational): Scalar = copy(factor = factor*rhs, shift = shift*rhs)
    def /(rhs: Rational): Scalar = copy(factor = factor/rhs, shift = shift/rhs)
    def +(rhs: Rational): Scalar = copy(shift = shift + rhs)
    def -(rhs: Rational): Scalar = copy(shift = shift - rhs)
  }

  /** Represents a value with an exact real cyclotomic number. */
  case class Exact(cyclo: RealCyclo) extends Scalar {
    override def toString = cyclo.toString
    def lowerBound = cyclo
    def upperBound = cyclo
    def midPoint = cyclo
    def toInterval = Interval.point(cyclo)
    def *(rhs: Rational): Scalar = Exact(cyclo * RealCyclo(rhs))
    def /(rhs: Rational): Scalar = Exact(cyclo / RealCyclo(rhs))
    def +(rhs: Rational): Scalar = Exact(cyclo + RealCyclo(rhs))
    def -(rhs: Rational): Scalar = Exact(cyclo - RealCyclo(rhs))
  }

}
