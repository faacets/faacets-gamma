package com.faacets
package operation

import cats.data.{Validated, ValidatedNel}
import spire.algebra.partial.{Groupoid, PartialAction}
import spire.algebra.Eq
import spire.syntax.eq._

import com.faacets.core._
import com.faacets.data._
import com.faacets.operation.lifting._

case class Lifting(source: Grouping, target: Grouping) {

  override def toString = s"$source -> $target"

  def nParties = source.parties.size

  require(source.minimalScenario === target.minimalScenario)

}

object Lifting {

  def validate(source: Grouping, target: Grouping): ValidatedNel[String, Lifting] =
    if (source.minimalScenario === target.minimalScenario) Validated.valid(apply(source, target))
    else Validated.invalidNel("Source and target groupings are not compatible")

  implicit val equ = Eq.fromUniversalEquals[Lifting]
  implicit val groupoid: Groupoid[Lifting] = new LiftingGroupoid
  implicit val textable: Textable[Lifting] = Textable.fromParser(Parsers.lifting, _.toString)
  implicit val exprAction: PartialAction[Expr, Lifting] = new LiftingExprPartialAction
  implicit val exprExtractor: OperationExtractor[Expr, Lifting] = new ExprLiftingExtractor
  implicit def boundedExprAction(implicit pb: PreservedBounds[Lifting]): PartialAction[BoundedExpr, Lifting] =
    BoundedExpr.constructPartialAction[Lifting](pb.boundTransform, pb.facetOfTransform)

}
