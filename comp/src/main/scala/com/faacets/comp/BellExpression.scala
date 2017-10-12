package com.faacets.comp

import scala.collection.immutable.{ListMap, ListSet}

import cats.data.{Validated, ValidatedNel}
import spire.algebra.partial.{Groupoid, PartialAction}
import spire.algebra.{Action, Group}
import spire.math.Rational
import spire.syntax.action._
import spire.syntax.group._
import spire.syntax.groupoid._
import spire.syntax.partialAction._
import spire.util.Opt
import scalin.immutable.Vec
import net.alasc.attributes.{Attributable, Attributes}
import net.alasc.finite.Grp
import net.alasc.perms.default._

import io.circe._
import io.circe.syntax._

import com.faacets.consolidate.instances.all._
import com.faacets.consolidate.syntax.all._
import com.faacets.consolidate.{Merge, Result}
import com.faacets.core._
import com.faacets.data.instances.all._
import com.faacets.data.syntax.all._
import com.faacets.operation._
import com.faacets.operation.instances.all._

case class BellExpression(boundedExpr: BoundedExpr,
                          display: Option[Display] = None,
                          shortName: Option[String] = None,
                          description: Option[String] = None) extends Attributable {

  def expr: Expr = boundedExpr.expr
  def lower: LowerOrientation = boundedExpr.lower
  def upper: UpperOrientation = boundedExpr.upper

  def decomposition: PolyProduct[CanonicalDec[Expr]] = BellExpression.attributes.decomposition(this) {
    ProductExtractor[Expr].forceExtract(expr)
      .mapAffine(e => CanonicalWithAffineExtractor[Expr].apply(e).splitAffine)
  }

  def canonicalComponents: Vector[BellExpression] =
    decomposition.toSingleOption.fold {
      // if it is a product
      val canonicalExprs = decomposition.components.values.map(_.canonical).toVector
      canonicalExprs.map( expr => BellExpression(BoundedExpr(expr)) )
    } {
      case (affine, CanonicalDec(lifting, reordering, relabeling, canonical)) =>
        val undoAffine = this <|+| affine.inverse
        val undoLifting = lifting.fold(undoAffine)(l => (undoAffine <|+|? l.inverse).get)
        val undoReordering = reordering.fold(undoLifting)(r => (undoLifting <|+|? r.inverse). get)
        val undoRelabeling = (undoReordering <|+|? relabeling).get
        Vector(undoRelabeling)
    }

}

object BellExpression {

  object attributes extends Attributes("BellExpression") {

    object decomposition extends Attribute.OfValue[PolyProduct[CanonicalDec[Expr]]]("decomposition")

  }

  implicit def constructPartialAction[O:Groupoid](implicit exprPA: PartialAction[BoundedExpr, O]): PartialAction[BellExpression, O] =
    new PartialAction[BellExpression, O] {

      def partialActr(be: BellExpression, o: O): Opt[BellExpression] =
        exprPA.partialActr(be.boundedExpr, o).map(BellExpression(_))

      def partialActl(o: O, be: BellExpression): Opt[BellExpression] =
        exprPA.partialActl(o, be.boundedExpr).map(BellExpression(_))

    }

  implicit def constructAction[O:Group](implicit exprA: Action[BoundedExpr, O]): Action[BellExpression, O] =
    new Action[BellExpression, O] {

      def actr(be: BellExpression, o: O): BellExpression =
        BellExpression(exprA.actr(be.boundedExpr, o))

      def actl(o: O, be: BellExpression): BellExpression =
        BellExpression(exprA.actl(o, be.boundedExpr))

    }

  implicit val tensor: Tensor[BellExpression] = new Tensor[BellExpression] {
    def apply(components: Map[Set[Int], BellExpression]): BellExpression =
      BellExpression(Tensor[BoundedExpr].apply(components.mapValues(_.boundedExpr)))
  }

  def fromExpr(expr: Expr): BellExpression =
    BellExpression(BoundedExpr(expr))

  def validate(boundedExpr: BoundedExpr,
               display: Option[Display],
               shortName: Option[String],
               description: Option[String],
               decomposition: Option[PolyProduct[CanonicalDec[Expr]]]
              ): ValidatedNel[String, BellExpression] = {
    val res = BellExpression(boundedExpr, display, shortName, description)
    decomposition.foreach( d => BellExpression.attributes.decomposition(res)(d) )
    Validated.valid(res)
  }

  implicit val merge: Merge[BellExpression] = new Merge[BellExpression] {

    def merge(base: BellExpression, other: BellExpression): Result[BellExpression] = {
      val boundedExpr = base.boundedExpr merge other.boundedExpr
      val display = base.display merge other.display
      implicit val stringMerge: Merge[String] = Merge.fromEquals[String]
      val shortName = base.shortName merge other.shortName
      val description = base.description merge other.description
      import BellExpression.attributes.{decomposition => dc}
      implicit object DecompositionMerge extends Merge[PolyProduct[CanonicalDec[Expr]]] {
        def merge(base: PolyProduct[CanonicalDec[Expr]], other: PolyProduct[CanonicalDec[Expr]]): Result[PolyProduct[CanonicalDec[Expr]]] =
          Result.same(base)
      }
      val decomposition = dc.get(base) merge dc.get(other)
      import cats.syntax.all._
      (boundedExpr |@| display |@| shortName |@| description |@| decomposition).map( (_,_,_,_,_) )
        .validate((BellExpression.validate _).tupled)
    }

  }

  implicit class RichListMap[K, V](val listMap: ListMap[K, V]) extends AnyVal {
    def mapKeys[K1](f: K => K1): ListMap[K1, V] =
      listMap.map { case (k, v) => (f(k), v) }
  }
   implicit lazy val encode: Encoder[BellExpression] = Encoder.instance[BellExpression] { be =>
    Json.obj(
      "shortName" -> be.shortName.asJson,
      "scenario" -> be.expr.scenario.asJson,
      "coefficients" -> be.expr.coefficients.asJson,
      "symmetryGroup" -> NDVec.attributes.symmetryGroup.get(be.expr).fold(Json.Null)(_.asJson),
      "display" -> be.display.asJson,
      "lower" -> (if (be.lower.isEmpty) Json.Null else be.lower.asJson),
      "upper" -> (if (be.upper.isEmpty) Json.Null else be.upper.asJson),
      "description" -> be.description.fold(Json.Null)(_.asJson),
      "decomposition" -> BellExpression.attributes.decomposition.get(be).fold(Json.Null)(_.asJson)
    )
  }

  implicit lazy val decode: Decoder[BellExpression] = new Decoder[BellExpression] {

    def apply(c: HCursor): Decoder.Result[BellExpression] = decodeAccumulating(c).leftMap(_.head).toEither

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[BellExpression] =
      AccumulatingDecoder.resultInstance.map9(
        Decoder[Option[String]].tryDecodeAccumulating(c.downField("shortName")),

        Decoder[Scenario].tryDecodeAccumulating(c.downField("scenario")),
        Decoder[Vec[Rational]].tryDecodeAccumulating(c.downField("coefficients")),
        Decoder[Option[Grp[Relabeling]]].tryDecodeAccumulating(c.downField("symmetryGroup")),

        Decoder[Option[Display]].tryDecodeAccumulating(c.downField("display")),

        Decoder[Option[LowerOrientation]].tryDecodeAccumulating(c.downField("lower")).map(_.getOrElse(LowerOrientation.empty)),
        Decoder[Option[UpperOrientation]].tryDecodeAccumulating(c.downField("upper")).map(_.getOrElse(UpperOrientation.empty)),

        Decoder[Option[String]].tryDecodeAccumulating(c.downField("description")),

        Decoder[Option[PolyProduct[CanonicalDec[Expr]]]].tryDecodeAccumulating(c.downField("decomposition"))
      )( (_,
        _, _, _,
        _,
        _, _,
        _,
        _) ).andThen {
        case (sn: Option[String],
        s: Scenario, c: Vec[Rational], sg: Option[Grp[Relabeling]],
        d: Option[Display],
        lower: LowerOrientation, upper: UpperOrientation,
        dsc: Option[String],
        decomposition: Option[PolyProduct[CanonicalDec[Expr]]]) =>
          Expr.validate(s, c, sg).toAccumulatingDecoderResult
            .andThen { expr =>
              BoundedExpr.validate(expr, lower, upper).toAccumulatingDecoderResult
                .map(bde => BellExpression(boundedExpr = bde, display = d, shortName = sn, description = dsc))
            }
      }
  }

  /** Addition of BellExpression. Does not recover bounds, TODO */
  implicit val additiveGroupoid: AdditiveGroupoid[BellExpression] = AdditiveGroupoid(new Groupoid[BellExpression] {
    def inverse(a: BellExpression): BellExpression =
      BellExpression(AdditiveGroupoid[BoundedExpr].groupoid.inverse(a.boundedExpr))
    def partialOp(x: BellExpression, y: BellExpression): Opt[BellExpression] = {
      AdditiveGroupoid[BoundedExpr].groupoid.partialOp(x.boundedExpr, y.boundedExpr).map(BellExpression(_))
    }
  })

}
