package com.faacets
package operation
package reordering
/*
import spire.algebra.{Eq, Groupoid, NullboxPartialAction}
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.eq._
import spire.syntax.cfor._
import spire.util.Nullbox

import data._

import core._
import core.perm._

import qalg.immutable.QVector

final class VecReorderingAction[V <: Vec[V]](implicit builder: VecBuilder[V]) extends NullboxPartialAction[V, Reordering] {

  import Reordering.reorderParty

  def partialActl(r: Reordering, v: V): Nullbox[V] = partialActr(v, r.inverse)
  def partialActr(v: V, r: Reordering): Nullbox[V] = {
    if (v.scenario =!= r.source) return Nullbox.empty[V]

    v.representation match {
      case SPRepresentation | NPRepresentation =>
        val n = r.source.parties.size
        val sourceCompactParties = r.source.parties.map(reorderParty)
        val targetCompactParties = r.target.parties.map(reorderParty)

        def matchSeq[A](remSeqA: List[A], remSeqB: Map[A, Set[Int]]): List[Int] = remSeqA match {
          case hd :: tl =>
            val set = remSeqB(hd)
            val ind = set.min
            ind :: matchSeq(tl, remSeqB.updated(hd, set - ind))
          case Nil => Nil
        }

        def indices[A](seq: Seq[A]): Map[A, Set[Int]] =
          seq.zipWithIndex.groupBy(_._1).mapValues(_.map(_._2).toSet)

        val sourcePartyForTargetParty = matchSeq(targetCompactParties.toList, indices(sourceCompactParties))
        val reorderedParties = (0 until n).map(p => r.source.parties(sourcePartyForTargetParty(p)) )

        val sourceInputForTargetInputSeq: Seq[Seq[Int]] = (0 until n).map { p =>
          val sourceInputs = reorderedParties(p).inputs
          val targetInputs = r.target.parties(p).inputs

          matchSeq(targetInputs.toList, indices(sourceInputs))
        }

        val sourceAs = new Array[Int](n)
        val sourceXs = new Array[Int](n)

        Nullbox(builder.buildSP(r.target) { (targetAs, targetXs) =>
          cforRange(0 until n) { p =>
            val sourceA = targetAs(p)
            val sourceX = sourceInputForTargetInputSeq(p)(targetXs(p))
            sourceAs(sourcePartyForTargetParty(p)) = sourceA
            sourceXs(sourcePartyForTargetParty(p)) = sourceX
          }
          v.coefficientP(sourceAs, sourceXs)
        }.to(v.representation))
      case NGRepresentation | NCRepresentation | SGRepresentation | SCRepresentation =>
        partialActr(v.to(v.representation.permutable), r).map(_.to(v.representation))
      case _ => throw new IllegalArgumentException("Reordering is not yet supported on strategies.")
    }
  }
}

final class VecReorderingExtractor[V <: Vec[V]](implicit val action: NullboxPartialAction[V, Reordering]) extends OperationExtractor[V, Reordering] {
  def groupoid = Reordering.Groupoid
  def identity(v: V) = Reordering.ScenarioReorderingExtractor.identity(v.scenario)
  def partialExtract(v: V) = Reordering.ScenarioReorderingExtractor.partialExtract(v.scenario)
}
*/