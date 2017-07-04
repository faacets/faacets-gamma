package com.faacets.comp

import cats.data.{Validated, ValidatedNel}
import com.faacets.core.{Expr, NDVec, Relabeling, Scenario}
import com.faacets.operation._
import io.circe._
import net.alasc.attributes.{Attributable, Attributes}
import net.alasc.finite.Grp
import com.faacets.data.instances.all._
import com.faacets.data.syntax.all._
import net.alasc.perms.default._
import scalin.immutable.Vec
import spire.math.Rational
import io.circe.syntax._

case class BellExpression(boundedExpr: BoundedExpr,
                          display: Option[Display],
                          keywords: Set[String] = Set.empty[String],
                          shortName: Option[String] = None,
                          names: Seq[String] = Seq.empty[String],
                          sources: Map[String, Seq[String]] = Map.empty[String, Seq[String]]) extends Attributable {

  def expr: Expr = boundedExpr.expr
  def lower: LowerOrientation = boundedExpr.lower
  def upper: UpperOrientation = boundedExpr.upper

}

object BellExpression {

  def validate(boundedExpr: BoundedExpr,
               display: Option[Display],
               keywords: Set[String],
               shortName: Option[String],
               names: Seq[String],
               sources: Map[String, Seq[String]],
               decomposition: Option[PolyProduct[CanonicalDec[Expr]]]
              ): ValidatedNel[String, BellExpression] = {
    val res = BellExpression(boundedExpr, display, keywords, shortName, names, sources)
    decomposition.foreach( d => BellExpression.attributes.decomposition(res)(d) )
    Validated.valid(res)
  }

  object attributes extends Attributes("BellExpression") {

    object decomposition extends Attribute.OfValue[PolyProduct[CanonicalDec[Expr]]]("decomposition")

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
      "names" -> (if (be.names.isEmpty) Json.Null else be.names.asJson),
      "sources" -> (if (be.sources.isEmpty) Json.Null else be.sources.asJson),
      "decomposition" -> BellExpression.attributes.decomposition.get(be).fold(Json.Null)(_.asJson)
    )
  }

  implicit lazy val decode: Decoder[BellExpression] = new Decoder[BellExpression] {

    def apply(c: HCursor): Decoder.Result[BellExpression] = decodeAccumulating(c).leftMap(_.head).toEither

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[BellExpression] =
      AccumulatingDecoder.resultInstance.map10(
        Decoder[Option[String]].tryDecodeAccumulating(c.downField("shortName")),

        Decoder[Scenario].tryDecodeAccumulating(c.downField("scenario")),
        Decoder[Vec[Rational]].tryDecodeAccumulating(c.downField("coefficients")),
        Decoder[Option[Grp[Relabeling]]].tryDecodeAccumulating(c.downField("symmetryGroup")),

        Decoder[Option[Display]].tryDecodeAccumulating(c.downField("display")),

        Decoder[Option[LowerOrientation]].tryDecodeAccumulating(c.downField("lower")).map(_.getOrElse(LowerOrientation.empty)),
        Decoder[Option[UpperOrientation]].tryDecodeAccumulating(c.downField("upper")).map(_.getOrElse(UpperOrientation.empty)),

        Decoder[Option[Seq[String]]].tryDecodeAccumulating(c.downField("names")).map(_.getOrElse(Seq.empty[String])),
        Decoder[Option[Map[String, Seq[String]]]].tryDecodeAccumulating(c.downField("sources")).map(_.getOrElse(Map.empty[String, Seq[String]])),

        Decoder[Option[PolyProduct[CanonicalDec[Expr]]]].tryDecodeAccumulating(c.downField("decomposition"))
      )( (_,
        _, _, _,
        _,
        _, _,
        _, _,
        _) ).andThen {
        case (sn: Option[String],
        s: Scenario, c: Vec[Rational], sg: Option[Grp[Relabeling]],
        d: Option[Display],
        lower: LowerOrientation, upper: UpperOrientation,
        names: Seq[String], sources: Map[String, Seq[String]],
        decomposition: Option[PolyProduct[CanonicalDec[Expr]]]) =>
          Expr.validate(s, c, sg).toAccumulatingDecoderResult
            .andThen { expr =>
              BoundedExpr.validate(expr, lower, upper).toAccumulatingDecoderResult
                .map(bde => BellExpression(boundedExpr = bde, display = d, shortName = sn, names = names, sources = sources))
            }
      }
  }

}
