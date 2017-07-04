package com.faacets.comp

import com.faacets.core.Scenario
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import scalin.immutable.Vec
import io.circe.syntax._
import com.faacets.data.instances.all._
import com.faacets.data.syntax.all._
import scalin.immutable.dense._
import spire.math.Rational

sealed trait Display

object Display {

  case class CollinsGisin(coeffs: Vec[Rational]) extends Display

  case class Correlators(coeffs: Vec[Rational]) extends Display

  case class SignalingProbabilities(coeffs: Vec[Rational]) extends Display

  implicit val encoder: Encoder[Display] = Encoder.instance { d =>
    d match {
      case CollinsGisin(c) => Json.obj("type" -> Json.fromString("CollinsGisin"), "coefficients" -> c.asJson)
      case Correlators(c) => Json.obj("type" -> Json.fromString("Correlators"), "coefficients" -> c.asJson)
      case SignalingProbabilities(c) => Json.obj("type" -> Json.fromString("SignalingProbabilities"), "coefficients" -> c.asJson)
    }
  }

  implicit val decoder: Decoder[Display] = Decoder.instance { h =>
    h.downField("type").as[String].flatMap {
      case "CollinsGisin" => h.downField("coefficients").as[Vec[Rational]].map(c => CollinsGisin(c))
      case "Correlators" => h.downField("coefficients").as[Vec[Rational]].map(c => Correlators(c))
      case "SignalingProbabilities" => h.downField("coefficients").as[Vec[Rational]].map(c => SignalingProbabilities(c))
      case t => Left(DecodingFailure(s"Unknown display type $t", h.history))
    }
  }

}
