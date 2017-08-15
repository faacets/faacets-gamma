package com.faacets.comp

import scala.collection.immutable.{ListMap, ListSet}

import cats.data.{Validated, ValidatedNel}
import cats.instances.all._
import cats.syntax.all._
import spire.math.Rational
import scalin.immutable.Vec

import io.circe.{AccumulatingDecoder, Decoder, HCursor}

import com.faacets.core.{DExpr, Expr, Scenario}
import com.faacets.data.Value
import com.faacets.data.instances.all._
import com.faacets.operation._

case class LegacyBellExpression(scenario: Scenario,
                                representation: String,
                                coefficients: Vec[Rational],
                                lower: LegacyOrientation,
                                upper: LegacyOrientation,
                                keywords: ListSet[String],
                                shortName: Option[String],
                                names: ListSet[String],
                                sources: ListMap[String, ListSet[String]]
                               ) {

  def recoverExprAndDisplay: ValidatedNel[String, (Expr, Option[Display])] = representation match {
    case "Non-signaling Collins-Gisin" =>
      Expr.validateCollinsGisin(scenario, coefficients).map { expr =>
        (expr, Some(Display.CollinsGisin(coefficients)))
      }
    case "Non-signaling Correlators" =>
      Expr.validateCorrelators(scenario, coefficients).map { expr =>
        (expr, Some(Display.Correlators(coefficients)))
      }
    case "Signaling Probabilities" =>
      DExpr.validate(scenario, coefficients).map { dExpr =>
        (dExpr.projected, Some(Display.SignalingProbabilities(coefficients)))
      }
    case "Non-signaling Probabilities" =>
      Expr.validate(scenario, coefficients).map( (_, None) )
  }

  def toBellExpression: ValidatedNel[String, BellExpression] =
    (recoverExprAndDisplay |@| lower.toLowerOrientation |@| upper.toUpperOrientation).map( (_,_,_) ).andThen {
      case ((expr, display), l, u) => BoundedExpr.validate(expr, l, u).map( (_, display) )
    } map { case (boundedExpr, display) => BellExpression(boundedExpr, display, ListSet.empty[String], shortName, names, sources) }

}

object LegacyBellExpression {

  implicit lazy val decode: Decoder[LegacyBellExpression] = new Decoder[LegacyBellExpression] {

    def apply(c: HCursor): Decoder.Result[LegacyBellExpression] = decodeAccumulating(c).leftMap(_.head).toEither
    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[LegacyBellExpression] =
      AccumulatingDecoder.resultInstance.map9(
        Decoder[Scenario].tryDecodeAccumulating(c.downField("scenario")),
        Decoder[String].tryDecodeAccumulating(c.downField("representation")),
        Decoder[Vec[Rational]].tryDecodeAccumulating(c.downField("coefficients")),
        Decoder[Option[LegacyOrientation]].tryDecodeAccumulating(c.downField("lower")).map(_.getOrElse(LegacyOrientation.empty)),
        Decoder[Option[LegacyOrientation]].tryDecodeAccumulating(c.downField("upper")).map(_.getOrElse(LegacyOrientation.empty)),
        Decoder[Option[ListSet[String]]].tryDecodeAccumulating(c.downField("keywords")).map(_.getOrElse(ListSet.empty[String])),
        Decoder[Option[String]].tryDecodeAccumulating(c.downField("shortName")),
        Decoder[Option[ListSet[String]]].tryDecodeAccumulating(c.downField("names")).map(_.getOrElse(ListSet.empty[String])),
        Decoder[Option[ListMap[String, ListSet[String]]]].tryDecodeAccumulating(c.downField("sources")).map(_.getOrElse(ListMap.empty[String, ListSet[String]]))
      )( LegacyBellExpression.apply )
  }

}

case class LegacyOrientation(bounds: ListMap[String, Value], keywords: ListSet[String]) {
  import LegacyOrientation.FacetPattern

  def validateKeywords: ValidatedNel[String, ListMap[String, Boolean]] = {
    val allFacetOfs = keywords.toVector.map {
      case FacetPattern(name) =>
        if (bounds.contains(name)) Validated.Valid(name -> true) else Validated.invalidNel(s"Facet keyword for absent bound $name")
      case k => Validated.invalidNel(s"Unknown keyword $k")
    }
    (allFacetOfs.sequenceU: ValidatedNel[String, Vector[(String, Boolean)]]).map(ListMap(_: _*))
  }

  def toLowerOrientation: ValidatedNel[String, LowerOrientation] =
    validateKeywords.andThen ( fo => LowerOrientation.validate(bounds, fo) )

  def toUpperOrientation: ValidatedNel[String, UpperOrientation] =
    validateKeywords.andThen ( fo => UpperOrientation.validate(bounds, fo) )

}

object LegacyOrientation {

  val empty = LegacyOrientation(ListMap.empty[String, Value], ListSet.empty[String])

  val FacetPattern = "facet-([a-zA-Z]+)".r

  implicit val decoder: Decoder[LegacyOrientation] = new Decoder[LegacyOrientation] {
    def apply(c: HCursor): Decoder.Result[LegacyOrientation] = decodeAccumulating(c).leftMap(_.head).toEither
    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[LegacyOrientation] =
      AccumulatingDecoder.resultInstance.map2(
        Decoder[Option[ListMap[String, Value]]].tryDecodeAccumulating(c.downField("bounds")).map(_.getOrElse(ListMap.empty[String, Value])),
        Decoder[Option[ListSet[String]]].tryDecodeAccumulating(c.downField("keywords")).map(_.getOrElse(ListSet.empty[String]))
      )( (_, _) ).map {
        case (b: ListMap[String, Value], k: ListSet[String]) => LegacyOrientation(b, k)
      }
  }

}
