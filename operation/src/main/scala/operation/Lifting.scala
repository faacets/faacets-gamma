package com.faacets
package operation
/*
import spire.algebra.{Groupoid, PartialAction}
import spire.syntax.eq._

import net.alasc.math.{Grp, Perm, Domain}

import data._

import core._
import core.perm.{Relabeling, PartyRelabeling}
import util._
import lifting._

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
  implicit val Eq = spire.optional.genericEq.generic[Lifting]
  implicit val Parsable: Parsable[Lifting] = new LiftingParsable
  implicit val Groupoid: Groupoid[Lifting] = new LiftingGroupoid
  implicit val ExprAction: PartialAction[Expr, Lifting] = new LiftingExprAction
  implicit val ExprExtractor: OperationExtractor[Expr, Lifting] = new ExprLiftingExtractor
}
*/