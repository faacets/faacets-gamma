package com.faacets.operation

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import com.faacets.consolidate.{Merge, Result}
import com.faacets.core.{Expr, NDVec, Relabeling, Scenario}
import com.faacets.data.Value
import com.faacets.data.instances.all._
import com.faacets.data.syntax.all._
import io.circe.{AccumulatingDecoder, Decoder, Encoder, HCursor}
import com.faacets.consolidate.instances.all._
import net.alasc.perms.default._
import net.alasc.finite.Grp
import scalin.immutable.Vec
import spire.math.Rational
import spire.math.interval.Overlap

import scala.collection.immutable.{ListMap, ListSet}

case class BellExpression(expr: Expr,
                          lower: LowerOrientation,
                          upper: UpperOrientation
                         )

object BellExpression {

  def validate(expr: Expr, lower: LowerOrientation, upper: UpperOrientation): ValidatedNel[String, BellExpression] =
    Validated.Valid(BellExpression(expr, lower, upper))

  implicit lazy val encode: Encoder[BellExpression] = Encoder.forProduct5[Scenario, Vec[Rational], Option[Grp[Relabeling]], LowerOrientation, UpperOrientation, BellExpression](
    "scenario", "coefficients", "symmetryGroup", "lower", "upper"
  )( (b: BellExpression) => (b.expr.scenario, b.expr.coefficients, NDVec.attributes.symmetryGroup.get(b.expr), b.lower, b.upper))

  implicit lazy val decode: Decoder[BellExpression] = new Decoder[BellExpression] {

    def apply(c: HCursor): Decoder.Result[BellExpression] = decodeAccumulating(c).leftMap(_.head).toEither

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[BellExpression] =
      AccumulatingDecoder.resultInstance.map5(
        Decoder[Scenario].tryDecodeAccumulating(c.downField("scenario")),
        Decoder[Vec[Rational]].tryDecodeAccumulating(c.downField("coefficients")),
        Decoder[Option[Grp[Relabeling]]].tryDecodeAccumulating(c.downField("symmetryGroup")),
        Decoder[LowerOrientation].tryDecodeAccumulating(c.downField("lower")),
        Decoder[UpperOrientation].tryDecodeAccumulating(c.downField("upper"))
      )( (_, _, _, _, _) ).andThen {
        case (s: Scenario, c: Vec[Rational], sg: Option[Grp[Relabeling]], lower: LowerOrientation, upper: UpperOrientation) =>
          Expr.validate(s, c, sg).toAccumulatingDecoderResult
            .andThen { expr => BellExpression.validate(expr, lower, upper).toAccumulatingDecoderResult }
      }
  }

}

sealed trait Orientation[O <: Orientation[O, N], N <: Orientation[N, O]] {
  def builder: OrientationBuilder[O]
  def bounds: ListMap[String, Value]
  def facetOf: ListMap[String, Boolean]
  def opposite: N
}

trait OrientationBuilder[O <: Orientation[O, _]] { self =>

  /** Lists all bounds which obey an ordering relation. Contains (b1, b2) if b1 <= b2. */
  def boundLTE: Set[(String, String)]

  def apply(bounds: ListMap[String, Value], facetOf: ListMap[String, Boolean]): O

  def validate(bounds: ListMap[String, Value], facetOf: ListMap[String, Boolean]): ValidatedNel[String, O] = {
    val inconsistencies = boundLTE.toList.flatMap {
      case (l, u) => (bounds.get(l), bounds.get(u)) match {
        case (Some(l1), Some(u1)) =>
          val l2 = l1.toRealCycloInterval
          val u2 = u1.toRealCycloInterval
          l2.overlap(u2) match {
            case Overlap.Disjoint(l3, u3) =>
              if ((l2 == l3) && (u2 == u3)) None else Some(s"$l bound and $u bound are inconsistent")
            case _ => None
          }
        case _ => None
      }
    }
    NonEmptyList.fromList(inconsistencies).fold(Validated.Valid(apply(bounds, facetOf)): ValidatedNel[String, O])(Validated.invalid(_))
  }

  implicit val encode: Encoder[O] = Encoder.forProduct2("bounds", "facetOf")(
    (o: O) => (o.bounds: ListMap[String, Value], o.facetOf: ListMap[String, Boolean])
  )

  implicit val decode: Decoder[O] = new Decoder[O] {

    def apply(c: HCursor): Decoder.Result[O] = decodeAccumulating(c).leftMap(_.head).toEither

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[O] =
      AccumulatingDecoder.resultInstance.map2(
        Decoder[ListMap[String, Value]].tryDecodeAccumulating(c.downField("bounds")),
        Decoder[ListMap[String, Boolean]].tryDecodeAccumulating(c.downField("facetOf"))
       )( (_, _) ).andThen {
        case (b: ListMap[String, Value], f: ListMap[String, Boolean]) => self.validate(b, f).toAccumulatingDecoderResult
      }

  }

  implicit val merge: Merge[O] = new Merge[O] {

    def merge(base: O, newO: O): Result[O] = {
      import spire.std.boolean._
      implicit val mergeBoolean: Merge[Boolean] = Merge.fromEq[Boolean]
      import cats.syntax.all._
      import com.faacets.consolidate.syntax.all._
      val bounds = (base.bounds merge newO.bounds)
      val facetOf = (base.facetOf merge newO.facetOf)
      (bounds |@| facetOf).map( (_, _) ).validate( (validate _).tupled )
    }

  }

}

case class UpperOrientation(bounds: ListMap[String, Value], facetOf: ListMap[String, Boolean]) extends Orientation[UpperOrientation, LowerOrientation] {
  def builder = UpperOrientation
  def opposite: LowerOrientation = LowerOrientation(bounds.map { case (k,v) => (k, v.opposite) }, facetOf)
}

object UpperOrientation extends OrientationBuilder[UpperOrientation] {
  lazy val boundLTE = Set(("local", "quantum"), ("local", "nonsignaling"), ("quantum", "nonsignaling"))
}

case class LowerOrientation(bounds: ListMap[String, Value], facetOf: ListMap[String, Boolean]) extends Orientation[LowerOrientation, UpperOrientation] {
  def builder = LowerOrientation
  def opposite: UpperOrientation = UpperOrientation(bounds.map { case (k,v) => (k, v.opposite) }, facetOf)
}

object LowerOrientation extends OrientationBuilder[LowerOrientation] {
  lazy val boundLTE = UpperOrientation.boundLTE.map(_.swap)
}
