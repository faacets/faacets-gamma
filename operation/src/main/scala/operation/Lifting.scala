package com.faacets
package operation

import cats.data.{Validated, ValidatedNel}
import spire.algebra.Eq
import spire.syntax.eq._
import data._
import core._

import lifting._
import spire.algebra.partial.{Groupoid, PartialAction}

case class LiftingSplit(inputLifting: Lifting, outputLifting: Lifting) {
  require(inputLifting.target === outputLifting.source)
}

case class PartyLifting(source: PartyGrouping, target: PartyGrouping) {

  def intermediate: PartyGrouping = PartyGrouping(
    (target.inputs zip target.compactFromIndex) map {
    case (inputGrouping, None) => inputGrouping
    case (_, Some(compactIndex)) => source.inputs(source.indexFromCompact(compactIndex))
    }
  )

}

case class Lifting(source: Grouping, target: Grouping) {

  override def toString = s"$source -> $target"
  def nParties = source.parties.size

  require(source.compact === target.compact)

  lazy val intermediate: Grouping = Grouping(
    (source.parties zip target.parties).map {
      case (sourceParty, targetParty) => PartyLifting(sourceParty, targetParty).intermediate
    }
  )

  def split: LiftingSplit = LiftingSplit(Lifting(source, intermediate), Lifting(intermediate, target))

}

object Lifting {

  def validate(source: Grouping, target: Grouping): ValidatedNel[String, Lifting] =
    if (source.compact === target.compact) Validated.valid(apply(source, target))
    else Validated.invalidNel("Source and target groupings are not compatible")

  implicit val equ = Eq.fromUniversalEquals[Lifting]
  implicit val groupoid: Groupoid[Lifting] = new LiftingGroupoid
  implicit val textable: Textable[Lifting] = Textable.fromParser(LiftingParsers.lifting, _.toString)

  implicit val ExprAction: PartialAction[Expr, Lifting] = new LiftingExprAction
  //implicit val ExprExtractor: OperationExtractor[Expr, Lifting] = new ExprLiftingExtractor*/
}

