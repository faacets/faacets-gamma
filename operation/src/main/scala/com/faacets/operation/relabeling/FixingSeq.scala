package com.faacets
package operation
package relabeling
/*
import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.collection.immutable.BitSet

import spire.algebra.Order
import spire.math.Sorting
import spire.syntax.action._
import spire.syntax.group._
import spire.util._

import net.alasc.algebra.{FaithfulPermutationAction, FiniteGroup, Subgroup}
import net.alasc.math._
import net.alasc.math.bsgs._
import net.alasc.math.bsgs.algorithms._
import net.alasc.syntax.check._
import net.alasc.syntax.subgroup._

object FixingSeq {
  // TODO: change pointSetsToTest to bitsets
  class FixingTest[P](level: Int, seq: Int => Int, pointSetsToTest: Array[Array[Int]])(implicit algebra: FiniteGroup[P]) extends SubgroupTest[P] {
    def test(b: Int, orbitImage: Int, currentG: P, node: Node[P])(implicit action: FaithfulPermutationAction[P]): Nullbox[FixingTest[P]] = {
      val pointSet = pointSetsToTest(level)
      if (seq(pointSet(0)) != seq(orbitImage))
        return Nullbox.empty[FixingTest[P]]
      if (pointSet.length > 1) {
        val nextG = node.u(b) |+| currentG
        var i = 1
        while (i < pointSet.length) {
          val k = pointSet(i)
          if (seq(k) != seq(k <|+| nextG))
            return Nullbox.empty[FixingTest[P]]
          i += 1
        }
      }
      Nullbox(new FixingTest[P](level + 1, seq, pointSetsToTest))
    }
  }
  def fixing[P](chain: Chain[P], n: Int, seq: Int => Int)(implicit algebra: FiniteGroup[P], alg: BasicAlgorithms[P]): Chain[P] = chain match {
    case node: Node[P] =>
      implicit def action = node.action
      val pointSetsToTest: Array[Array[Int]] = alg.basePointGroups(chain, n)
      def leaveInvariant(g: P): Boolean = {
        var i = 0
        while (i < n) {
          if (seq(i <|+| g) != seq(i))
            return false
          i += 1
        }
        true
      }
      alg.subgroupSearch(chain, leaveInvariant(_), new FixingTest(0, seq, pointSetsToTest)).toChain
    case term: Term[P] => term
  }
}
*/