package com.faacets
package operation
package reordering

import spire.syntax.groupoid._
import spire.syntax.eq._
import spire.syntax.cfor._
import spire.util.Opt
import core._
import spire.algebra.partial.PartialAction

final class VecReorderingPartialAction[V <: PVec[V]](implicit builder: PVecBuilder[V]) extends PartialAction[V, Reordering] {

  import Reordering.reorderParty

  def partialActl(r: Reordering, v: V): Opt[V] = partialActr(v, r.inverse)

  def partialActr(v: V, r: Reordering): Opt[V] =
    if (v.scenario =!= r.source) Opt.empty[V] else {
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

      val newCoeffs = r.target.tabulateP { (targetAs, targetXs) =>
        cforRange(0 until n) { p =>
          val sourceA = targetAs(p)
          val sourceX = sourceInputForTargetInputSeq(p)(targetXs(p))
          sourceAs(sourcePartyForTargetParty(p)) = sourceA
          sourceXs(sourcePartyForTargetParty(p)) = sourceX
        }
        v.coefficient(sourceAs, sourceXs)
      }
      Opt(builder.apply(r.target, newCoeffs))
    }

}

final class VecReorderingExtractor[V <: PVec[V]](implicit val partialAction: PartialAction[V, Reordering]) extends OperationExtractor[V, Reordering] {
  def groupoid = Reordering.groupoid
  def identity(v: V) = Reordering.scenarioReorderingExtractor.identity(v.scenario)
  def extractOperation(v: V) = Reordering.scenarioReorderingExtractor.extractOperation(v.scenario)
}
