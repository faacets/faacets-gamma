package com.faacets
package operation
package relabeling
/*
import spire.algebra.NullboxPartialAction
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._

import net.alasc.math.Perm
import net.alasc.syntax.shiftablePermutation._

import core._
import perm._

/** Partition
  * 
  * Describes a partition of a `Scenario` between the first party and the
  * remaining parties, as to quickly find the minimal lexicographic representative
  * of a `Vec` in the `NP` representation.
  */
case class Partitions[V <: Vec[V]](rParty: Party, cScenario: Scenario)(implicit ev: NullboxPartialAction[V, Relabeling]) {
  val singleParty = SingleParty.findMinimalPermutation(rParty)(_)
  val scenario = Scenario(Vector(rParty) ++ cScenario.parties)
  val rLength = rParty.shapeP.size
  val cLength = cScenario.shapeP.size
  val rRep = rParty.probabilityRepresentation
  val cRep = cScenario.probabilityRepresentation
  val rGrp = rParty.group
  val cGrp = cScenario.group

  def findMinimal(vec: V): Relabeling = {
    val canSwapWithAlice = scenario.parties.zipWithIndex.filter( _._1 == rParty ).map(_._2)
    val partyPermutations = canSwapWithAlice.map { k =>
      Relabeling(Map.empty[Int, PartyRelabeling], if (k == 0) Perm.Algebra.id else Perm(0, k))
    }
    val candidates = partyPermutations.map { partyP =>
      val permVec = vec <|+| partyP
      def matrix(r: Int, c: Int) = permVec.permutableIntegerArray(rLength * c + r)
      val (rPerm, cPerm) = MatrixAlgorithms.findMinimalPermutation(rLength, cLength, matrix, rGrp, cGrp, rRep, cRep, Some(singleParty))
      val Relabeling(prMap, pPerm1) = cPerm.inverse
      partyP |+| Relabeling(Map(0 -> rPerm.inverse) ++ prMap.map { case (k, v) => (k + 1, v) }, pPerm1 + 1)
    }
    val res = {
      import spire.std.int._
      import spire.compat._
      import spire.std.seq.SeqOrder
      candidates.minBy(p => ((vec <|+| p).permutableIntegerArray: Seq[Int]))
    }
    res
  }
}

/** Optimized version for two parties. */
case class Partition2Parties[V <: Vec[V]](rParty: Party, cParty: Party) {
  val singleParty = SingleParty.findMinimalPermutation(rParty)(_)
  val scenario = Scenario(Vector(rParty, cParty))
  val rLength = rParty.shapeP.size
  val cLength = cParty.shapeP.size
  val rRep = rParty.probabilityRepresentation
  val cRep = cParty.probabilityRepresentation
  val rGrp = rParty.group
  val cGrp = cParty.group

  def findMinimal(vec: V): Relabeling = {
    def matrix(r: Int, c: Int) = vec.permutableIntegerArray(rLength * c + r)
    def matrixT(r: Int, c: Int) = vec.permutableIntegerArray(cLength * r + c)

    val (rPermInv, cPermInv) = MatrixAlgorithms.findMinimalPermutation(rLength, cLength, matrix, rGrp, cGrp, rRep, cRep, Some(singleParty))

    if (rParty != cParty)
      Relabeling(Map(0 -> rPermInv.inverse, 1 -> cPermInv.inverse), Perm.Algebra.id)
    else {
      val (rPermInv1, cPermInv1) = MatrixAlgorithms.findMinimalPermutation(rLength, cLength, matrixT, rGrp, cGrp, rRep, cRep, Some(singleParty))
      def nonTransposedBest: Boolean = {
        cforRange(0 until cLength) { c =>
          cforRange(0 until rLength) { r =>
            val nonTval = matrix(rRep.action.actr(r, rPermInv), cRep.action.actr(c, cPermInv))
            val tVal = matrixT(rRep.action.actr(r, rPermInv1), cRep.action.actr(c, cPermInv1))
            val cmp = nonTval - tVal
            if (cmp < 0) return true
            if (cmp > 0) return false
          }
        }
        true
      }
      if (nonTransposedBest)
        Relabeling(Map(0 -> rPermInv.inverse, 1 -> cPermInv.inverse), Perm.Algebra.id)
      else
        Relabeling(Map(0 -> rPermInv1.inverse, 1 -> cPermInv1.inverse), Perm(0,1))
    }
  }
}

/** Finds the permutation to the minimal lexicographic representatives.
  * 
  * Uses a partition of the scenario (Alice, [Bob Charlie ...])
  * and tabulates the coefficients in a matrix to permute columns and
  * rows independently.
  */
object Partitions {
   def findMinimal[V <: Vec[V]](vec: V)(implicit ev: NullboxPartialAction[V, Relabeling]): Relabeling = {
     val parties = vec.scenario.parties
     parties.size match {
       case 2 =>
         Partition2Parties(parties(0), parties(1)).findMinimal(vec)
       case _ =>
         Partitions(parties.head, Scenario(parties.tail))(ev).findMinimal(vec)
     }
   }
}
*/