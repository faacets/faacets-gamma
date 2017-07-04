package com.faacets.comp

import com.faacets.core.{Expr, NDVec, Relabeling, Scenario}
import com.faacets.operation._
import io.circe._
import net.alasc.attributes.{Attributable, Attributes}
import net.alasc.finite.Grp
import com.faacets.data.instances.all._
import com.faacets.data.syntax.all._
import scalin.immutable.Vec
import spire.math.Rational
import io.circe.syntax._

case class BellExpression(boundedExpr: BoundedExpr,
                          keywords: Set[String] = Set.empty[String],
                          shortName: Option[String] = None,
                          names: Seq[String] = Seq.empty[String],
                          remarkableForms: Map[String, BellExpression],
                          sources: Map[String, Seq[String]]) extends Attributable {

  def expr: Expr = boundedExpr.expr
  def lower: LowerOrientation = boundedExpr.lower
  def upper: UpperOrientation = boundedExpr.upper

}

object BellExpression {

  object attributes extends Attributes("BellExpression") {

    object decomposition extends Attribute.OfValue[PolyProduct[CanonicalDec[BellExpression]]]("decomposition")

  }

  implicit lazy val encode: Encoder[BellExpression] = Encoder.instance[BellExpression] { be =>
    Json.obj(
      "scenario" -> be.expr.scenario.asJson,
      "coefficients" -> be.expr.coefficients.asJson,
      "symmetryGroup" -> NDVec.attributes.symmetryGroup.get(be.expr).fold(Json.Null)(_.asJson),
      "lower" -> (if (be.lower.isEmpty) Json.Null else be.lower.asJson),
      "upper" -> (if (be.upper.isEmpty) Json.Null else be.upper.asJson)
    )
  }

  implicit lazy val decode: Decoder[BellExpression] = new Decoder[BellExpression] {

    def apply(c: HCursor): Decoder.Result[BellExpression] = decodeAccumulating(c).leftMap(_.head).toEither

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[BellExpression] =
      AccumulatingDecoder.resultInstance.map5(
        Decoder[Scenario].tryDecodeAccumulating(c.downField("scenario")),
        Decoder[Vec[Rational]].tryDecodeAccumulating(c.downField("coefficients")),
        Decoder[Option[Grp[Relabeling]]].tryDecodeAccumulating(c.downField("symmetryGroup")),
        Decoder[Option[LowerOrientation]].tryDecodeAccumulating(c.downField("lower")).map(_.getOrElse(LowerOrientation.empty)),
        Decoder[Option[UpperOrientation]].tryDecodeAccumulating(c.downField("upper")).map(_.getOrElse(UpperOrientation.empty))
      )( (_, _, _, _, _) ).andThen {
        case (s: Scenario, c: Vec[Rational], sg: Option[Grp[Relabeling]], lower: LowerOrientation, upper: UpperOrientation) =>
          Expr.validate(s, c, sg).toAccumulatingDecoderResult
            .andThen { expr => BellExpression(BoundedExpr.validate(expr, lower, upper).toAccumulatingDecoderResult) }
      }
  }

}
