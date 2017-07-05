package com.faacets.comp

import cats.data.Validated.Valid
import cats.data.{Validated, ValidatedNel}
import com.faacets.core.{DExpr, Expr, Scenario}
import com.faacets.data.Value
import com.faacets.operation.{BoundedExpr, LowerOrientation, UpperOrientation}
import io.circe.Decoder
import scalin.immutable.Vec
import spire.math.Rational
import cats.syntax.all._
import cats.instances.all._

import scala.collection.immutable.{ListMap, ListSet}

case class LegacyBellExpression(scenario: Scenario,
                                representation: String,
                                coefficients: Vec[Rational],
                                lower: LegacyOrientation,
                                upper: LegacyOrientation,
                                shortName: Option[String],
                                keywords: ListSet[String],
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

/*  implicit val decoder: Decoder[LegacyBellExpression] = new Decoder[LegacyBellExpression] {

  }*/

}

case class LegacyOrientation(bounds: ListMap[String, Value], keywords: Set[String]) {
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

  val FacetPattern = "facet-([a-zA-Z]+)".r
/*
  implicit val decoder: Decoder[LegacyOrientation] = new Decoder[LegacyOrientation] {

  }
*/
}
