package com.faacets.comp

import cats.data.ValidatedNel
import com.faacets.core.Scenario
import com.faacets.operation.{LowerOrientation, UpperOrientation}
import io.circe.Decoder
import scalin.immutable.Vec
import spire.math.Rational

case class LegacyBellExpression(scenario: Scenario,
                                representation: String,
                                coefficients: Vec[Rational],
                                lower: LegacyOrientation,
                                upper: LegacyOrientation,
                                shortName: Option[String],
                                keywords: Set[String],
                                names: Seq[String],
                                sources: Map[String, String]
                               ) {
  def toBellExpression: ValidatedNel[String, BellExpression]
}

object LegacyBellExpression {

  implicit val decoder: Decoder[LegacyBellExpression] = new Decoder[LegacyBellExpression] {

  }

}

case class LegacyOrientation(bounds: Map[String, String], keywords: Set[String]) {
  def toLowerOrientation: ValidatedNel[String, LowerOrientation]
  def toUpperOrientation: ValidatedNel[String, UpperOrientation]
}

object LegacyOrientation {

  implicit val decoder: Decoder[LegacyOrientation] = new Decoder[LegacyOrientation] {

  }

}