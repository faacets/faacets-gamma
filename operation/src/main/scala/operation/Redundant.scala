package com.faacets
package operation
/*
import spire.syntax.vectorSpace._
import spire.algebra.{Group, Groupoid, PartialAction, Semigroup, VectorSpace}
import spire.math.Rational

import play.api.libs.json._
import play.api.libs.functional.syntax._

import qalg.immutable.QVector

import core._

import data._
import data.any._

import redundant._

case class Redundant(expr: Expr) {
  require(expr.toNonSignaling._1.coefficients.toIndexedSeq.forall(_ == Rational.zero))
}

object Redundant {
  implicit val Eq = spire.optional.genericEq.generic[Redundant]
  implicit val Groupoid: Groupoid[Redundant] = new RedundantGroupoid
  implicit val ExprAction: PartialAction[Expr, Redundant] = new ExprRedundantAction
  implicit val ExprExtractor: OperationExtractor[Expr, Redundant] = new ExprRedundantExtractor
  implicit val Format: Format[Redundant] = (
    (JsPath \ "scenario").format[Scenario] and
      (JsPath \ "representation").format[Representation] and
      (JsPath \ "coefficients").format[QVector]
  )((s, r, c) => Redundant(Expr(s, r, c)), r => (r.expr.scenario, r.expr.representation, r.expr.coefficients))
}
*/