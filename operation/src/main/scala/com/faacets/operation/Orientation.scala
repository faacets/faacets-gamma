package com.faacets.operation

import scala.collection.immutable.ListMap

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import spire.math.interval.Overlap

import io.circe._
import io.circe.syntax._

import com.faacets.consolidate.instances.all._
import com.faacets.consolidate.{Merge, Result}
import com.faacets.data.Value
import com.faacets.data.instances.all._
import com.faacets.data.syntax.all._

sealed trait Orientation[O <: Orientation[O, N], N <: Orientation[N, O]] {
  def isEmpty: Boolean = bounds.isEmpty && facetOf.isEmpty
  def builder: OrientationBuilder[O]
  def bounds: ListMap[String, Value]
  def facetOf: ListMap[String, Boolean]
  def opposite: N
  def filterBoundsAndFacetOf(preserved: Set[String]): O =
    builder.apply(
      bounds.filter { case (k,v) => preserved.contains(k) },
      facetOf.filter { case (k,v) => preserved.contains(k) }
    )
  def mapBounds(f: Value => Value): O =
    builder.apply(
      bounds.map { case (k,v) => (k, f(v)) },
      facetOf
    )
}

trait OrientationBuilder[O <: Orientation[O, _]] { self =>

  /** Lists all bounds which obey an ordering relation. Contains (b1, b2) if b1 <= b2. */
  def boundLTE: Set[(String, String)]

  def empty: O = apply(ListMap.empty[String, Value], ListMap.empty[String, Boolean])

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

  implicit val encode: Encoder[O] = Encoder.instance[O] { o =>
    Json.obj(
      "bounds" -> (if (o.bounds.isEmpty) Json.Null else o.bounds.asJson),
      "facetOf" -> (if (o.facetOf.isEmpty) Json.Null else o.facetOf.asJson)
    )
  }

  implicit val decode: Decoder[O] = new Decoder[O] {
    def apply(c: HCursor): Decoder.Result[O] = decodeAccumulating(c).leftMap(_.head).toEither
    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[O] =
      AccumulatingDecoder.resultInstance.map2(
        Decoder[Option[ListMap[String, Value]]].tryDecodeAccumulating(c.downField("bounds")).map(_.getOrElse(ListMap.empty[String, Value])),
        Decoder[Option[ListMap[String, Boolean]]].tryDecodeAccumulating(c.downField("facetOf")).map(_.getOrElse(ListMap.empty[String, Boolean]))
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
