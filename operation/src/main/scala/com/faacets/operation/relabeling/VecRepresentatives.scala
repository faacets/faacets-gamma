package com.faacets
package operation
package relabeling
/*
import spire.algebra.NullboxPartialAction

import net.alasc.math._
import net.alasc.util._
import enum._

import core._
import core.perm.Relabeling

class VecRepresentatives[V <: Vec[V]](implicit vPartialAction: NullboxPartialAction[V, Relabeling]) {
  def representatives(v: V): RepresentativesSeq[V, Relabeling] with RepresentativesHead[V, Relabeling] =
    new RepresentativesSeq[V, Relabeling] with RepresentativesHead[V, Relabeling] { rs =>
      val grp = v.scenario.group
      override lazy val symGrp = v.symmetryGroup
      implicit def actionTG = vPartialAction
      implicit def classTagG = grp.gClassTag
      val representation = if (v.representation.isCorrelation) v.scenario.probabilityRepresentation else v.scenario.strategyRepresentation
      val maxInt = v.permutableIntegerArray.max
      def tInt(idx: Int) = v.permutableIntegerArray(idx)
      val tLength = v.permutableIntegerArray.length
      val t = v
      def seqInt(seq: V, idx: Int): NNOption =
        if (idx < seq.permutableIntegerArray.length) NNSome(seq.permutableIntegerArray(idx)) else NNNone
      override def head: LexRepresentative[V, Relabeling] =
        if (v.scenario.nParties > 1)
          new LexRepresentative[V, Relabeling] {
            implicit val actionTG = rs.actionTG
            val element = Partitions.findMinimal(v)(actionTG)
            val original = v
            def rank = BigInt(0)
          }
          else super.head
    }
}
*/