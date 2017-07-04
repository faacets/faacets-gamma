package com.faacets.operation

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.kernel.Comparison
import com.faacets.consolidate.{Merge, Result}
import com.faacets.core._
import com.faacets.data.Value
import com.faacets.data.instances.all._
import com.faacets.operation.instances.relabeling._
import com.faacets.data.syntax.all._
import io.circe._
import io.circe.syntax._
import com.faacets.consolidate.instances.all._
import com.faacets.operation.product.BoundedExprTensor
import cyclo.RealCyclo
import net.alasc.perms.default._
import net.alasc.finite.Grp
import scalin.immutable.Vec
import spire.algebra.{Action, Group}
import spire.algebra.partial.{Groupoid, PartialAction}
import spire.math.Rational
import spire.syntax.partialAction._
import spire.syntax.action._
import spire.syntax.groupoid._
import spire.syntax.group._
import spire.math.interval.Overlap
import spire.util.Opt
import scalin.immutable.dense._

import scala.collection.immutable.{ListMap, ListSet}

case class BoundedExpr(expr: Expr,
                       lower: LowerOrientation = LowerOrientation.empty,
                       upper: UpperOrientation = UpperOrientation.empty
                         ) {

  def reconstructBounds: BoundedExpr = {
    val pp = ProductExtractor[BoundedExpr].forceExtract(BoundedExpr(expr))
      .mapAffine(be => CanonicalWithAffineExtractor[BoundedExpr].apply(be).splitAffine)
    val pprec = pp.map(_.map { be =>
      BoundedExpr.canonicals.get(be.expr) match {
        case Some(c) => c
        case None => be
      }
    })
    pprec.toProductTreeOption.fold(pp.map(_.original).original)(_.map(_.original).original)
  }

}

object BoundedExpr {

  val canonicalPositivity = BoundedExpr(
    Expr(Scenario(Seq(Party(Seq(2)))), Vec[Rational](-1,1)),
    LowerOrientation(ListMap("local" -> Value(-1), "quantum" -> Value(-1), "nonsignaling" -> Value(-1)), ListMap.empty[String, Boolean]),
    UpperOrientation(ListMap("local" -> Value(1), "quantum" -> Value(1), "nonsignaling" -> Value(1)), ListMap.empty[String, Boolean])
  )

  val canonicalCHSH = BoundedExpr(
    Expr(Scenario.CHSH, Vec[Rational](-1, 1, -1, 1, 1, -1, 1, -1, -1, 1, 1, -1, 1, -1, -1, 1)),
    LowerOrientation(ListMap("local" -> Value(-2), "quantum" -> Value(-RealCyclo.sqrt2*2), "nonsignaling" -> Value(-4)), ListMap("local" -> true)),
    UpperOrientation(ListMap("local" -> Value(2), "quantum" -> Value(RealCyclo.sqrt2*2), "nonsignaling" -> Value(4)), ListMap("local" -> true))
  )

  val canonicals: Map[Expr, BoundedExpr] = Map(canonicalPositivity.expr -> canonicalPositivity, canonicalCHSH.expr -> canonicalCHSH)

  val CH = BoundedExpr(
    Expr.collinsGisin(Scenario.CHSH, Vec[Rational](0,0,-1,-1,1,1,0,-1,1)),
    LowerOrientation.empty,
    UpperOrientation(ListMap("local" -> Value(0)), ListMap.empty[String, Boolean])
  )

  val stdPreserved = Set("local", "quantum", "nonsignaling")

  def constructPartialAction[O:Groupoid](preservedBoundsAndFacetOf: Set[String])
                                 (implicit exprPA: PartialAction[Expr, O],
                                  valueA: Action[Value, O]): PartialAction[BoundedExpr, O] =
    new PartialAction[BoundedExpr, O] {

      def partialActr(be: BoundedExpr, o: O): Opt[BoundedExpr] = {
        def valueF(v: Value): Value = v <|+| o
        val newLower = be.lower.filterBoundsAndFacetOf(preservedBoundsAndFacetOf).mapBounds(valueF)
        val newUpper = be.upper.filterBoundsAndFacetOf(preservedBoundsAndFacetOf).mapBounds(valueF)
        (be.expr <|+|? o) match {
          case Opt(newExpr) =>
            Opt(BoundedExpr(newExpr, newLower, newUpper))
          case _ => Opt.empty[BoundedExpr]
        }
      }

      def partialActl(o: O, be: BoundedExpr): Opt[BoundedExpr] = partialActr(be, o.inverse)

    }

  def constructAction[O:Group](preservedBoundsAndFacetOf: Set[String])
                                        (implicit exprA: Action[Expr, O],
                                         valueA: Action[Value, O]): Action[BoundedExpr, O] =
    new Action[BoundedExpr, O] {

      def actr(be: BoundedExpr, o: O): BoundedExpr = {
        def valueF(v: Value): Value = v <|+| o
        val newLower = be.lower.filterBoundsAndFacetOf(preservedBoundsAndFacetOf).mapBounds(valueF)
        val newUpper = be.upper.filterBoundsAndFacetOf(preservedBoundsAndFacetOf).mapBounds(valueF)
        val newExpr = be.expr <|+| o
        BoundedExpr(newExpr, newLower, newUpper)
      }

      def actl(o: O, be: BoundedExpr): BoundedExpr = actr(be, o.inverse)

    }

  implicit def constructExtractor[O:Groupoid](implicit O: OperationExtractor[Expr, O],
                                     pa: PartialAction[BoundedExpr, O]): OperationExtractor[BoundedExpr, O] =
    new OperationExtractor[BoundedExpr, O] {
      def partialAction: PartialAction[BoundedExpr, O] = pa
      def groupoid: Groupoid[O] = implicitly
      def identity(be: BoundedExpr): O = O.identity(be.expr)
      def extractOperation(be: BoundedExpr): Opt[O] = O.extractOperation(be.expr)
    }

  implicit val tensor: Tensor[BoundedExpr] = new BoundedExprTensor

  def validate(expr: Expr, lower: LowerOrientation, upper: UpperOrientation): ValidatedNel[String, BoundedExpr] =
    Validated.Valid(BoundedExpr(expr, lower, upper))

  implicit lazy val encode: Encoder[BoundedExpr] = Encoder.instance[BoundedExpr] { be =>
    Json.obj(
      "scenario" -> be.expr.scenario.asJson,
      "coefficients" -> be.expr.coefficients.asJson,
      "symmetryGroup" -> NDVec.attributes.symmetryGroup.get(be.expr).fold(Json.Null)(_.asJson),
      "lower" -> (if (be.lower.isEmpty) Json.Null else be.lower.asJson),
      "upper" -> (if (be.upper.isEmpty) Json.Null else be.upper.asJson)
    )
  }

  implicit lazy val decode: Decoder[BoundedExpr] = new Decoder[BoundedExpr] {
    def apply(c: HCursor): Decoder.Result[BoundedExpr] = decodeAccumulating(c).leftMap(_.head).toEither

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[BoundedExpr] =
      AccumulatingDecoder.resultInstance.map5(
        Decoder[Scenario].tryDecodeAccumulating(c.downField("scenario")),
        Decoder[Vec[Rational]].tryDecodeAccumulating(c.downField("coefficients")),
        Decoder[Option[Grp[Relabeling]]].tryDecodeAccumulating(c.downField("symmetryGroup")),
        Decoder[Option[LowerOrientation]].tryDecodeAccumulating(c.downField("lower")).map(_.getOrElse(LowerOrientation.empty)),
        Decoder[Option[UpperOrientation]].tryDecodeAccumulating(c.downField("upper")).map(_.getOrElse(UpperOrientation.empty))
      )( (_, _, _, _, _) ).andThen {
        case (s: Scenario, c: Vec[Rational], sg: Option[Grp[Relabeling]], lower: LowerOrientation, upper: UpperOrientation) =>
          Expr.validate(s, c, sg).toAccumulatingDecoderResult
            .andThen { expr => BoundedExpr.validate(expr, lower, upper).toAccumulatingDecoderResult }
      }
  }

  implicit val lexicographicOrder: LexicographicOrder[BoundedExpr] = new LexicographicOrder[BoundedExpr] {
    def partialComparison(x: BoundedExpr, y: BoundedExpr): Option[Comparison] =
      LexicographicOrder[Expr].partialComparison(x.expr, y.expr)
  }

  implicit val additiveGroupoid: AdditiveGroupoid[BoundedExpr] = AdditiveGroupoid(new Groupoid[BoundedExpr] {
    def inverse(a: BoundedExpr): BoundedExpr = BoundedExpr(AdditiveGroupoid[Expr].groupoid.inverse(a.expr))
    def partialOp(x: BoundedExpr, y: BoundedExpr): Opt[BoundedExpr] = {
      AdditiveGroupoid[Expr].groupoid.partialOp(x.expr, y.expr) match {
        case Opt(r) => Opt(BoundedExpr(r))
        case _ => Opt.empty[BoundedExpr]
      }
    }
  })

}

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
