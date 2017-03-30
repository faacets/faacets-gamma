package com.faacets
package operation
package relabeling
/*
import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.Action
import spire.syntax.cfor._
import spire.syntax.group._
import spire.syntax.action._
import spire.util._

import net.alasc.algebra.{FaithfulPermutationAction, FiniteGroup, InversePair, Representation}
import net.alasc.math.{Grp, Domain}
import net.alasc.math.bsgs.{Chain, Node, TrivialNode, Term}
import net.alasc.math.guide.{BaseGuideLex, BaseGuideSeq}
import net.alasc.math.enum.{Algorithms => EnumAlgorithms}
import net.alasc.syntax.finiteGroup._

object MatrixAlgorithms {
  /** Returns the row and columns permutations that brings a matrix to its minimal lexicographic representatives, when the matrix is enumerated in
    * row-major order.
    * 
    * @param nrows    Number of rows in the matrix
    * @param ncols    Number of columns in the matrix
    * @param matrix   Function that returns the element (r,c) of the matrix as an (ordered) integer >= 0
    * @param rGrp     Group whose action permutes the rows
    * @param cGrp     Group whose action permutes the columns
    * @param rRep     Permutation representation of the group `rGrp` on the rows of the matrix
    * @param cRep     Permtuation representation of the group `cGrp` on the columns of the matrix
    * @param firstCol Optional fast function to compute the minimal representative of a column under the full group rGrp.
    * 
    * @return permutations (rPerm, cPerm) such that matrix'(r, c) = matrix(r <|+| rPerm, cPerm <|+|) is lexicographically minimal when
    *         enumerated in row-major order
    */
  def findMinimalPermutation[R, C](nrows: Int, ncols: Int, matrix: (Int, Int) => Int, rGrp: Grp[R], cGrp: Grp[C], rRep: Representation[R], cRep: Representation[C], firstCol: Option[(Int => Int) => R] = None): (R, C) = {
    implicit def rFiniteGroup: FiniteGroup[R] = rGrp.algebra
    implicit def cFiniteGroup: FiniteGroup[C] = cGrp.algebra
    implicit def rAction: FaithfulPermutationAction[R] = rRep.action
    implicit def rClassTag: ClassTag[R] = rGrp.gClassTag
    implicit def cClassTag: ClassTag[C] = cGrp.gClassTag
    implicit def cAction: FaithfulPermutationAction[C] = cRep.action

    // keeps a flat copy of the best minimal representative found so far, valid for the columns up to minimalCorrectBefore
    val minimal = new Array[Int](nrows * ncols)
    var minimalCorrectBefore = 0
    cforRange(0 until nrows) { r =>
      cforRange(0 until ncols) { c =>
        minimal(c * nrows + r) = matrix(r, c)
      }
    }
    // for each column c, stores the subgroup of rGrp that leaves the columns 0 to c included invariant
    val minimalSub = new Array[Grp[R]](ncols)
    // permutations of rows and columns that give the best minimal representative stored in minimal
    var minimalR = rFiniteGroup.id
    var minimalC = cFiniteGroup.id
    // compares the column c1, with its rows permuted by rPerm1 to the minimal column stored at minimalC2 (convention: permutedseq(i) = seq(i <|+| perm))
    def compareToMinimal(c1: Int, rPerm1: R, minimalC2: Int): Int = {
      cforRange(0 until nrows) { r =>
        val c = matrix(r <|+| rPerm1, c1) - minimal(r + minimalC2 * nrows)
        if (c != 0) return c
      }
      0
    }
    // returns the intersection fo grp with the symmetry subgroup of the c-th column permuted by rPerm (convention: permutedseq(i) = seq(i <|+| perm))
    def symmetrySubgroup(grp: Grp[R], rPerm: R, c: Int) = {
      implicit def representations = grp.representations
      implicit def algorithms = grp.algorithms
      Grp.fromChain(FixingSeq.fixing(grp.chain(rRep), nrows, r => matrix(r <|+| rPerm, c)), Nullbox(rRep))
    }
    // recurses column by column to find the best minimal representative
    def rec(level: Int, toLevel: Int, rPerm: R, cPerm: C, rSubGrp: Grp[R], cChain: Chain[C], cSymGrp: Grp[C]): Unit = cChain match {
      case node: Node[C] if level <= toLevel =>
        val candidatesRPerm = debox.Buffer.empty[R]
        val candidatesCPerm = debox.Buffer.empty[C]
        val beta = node.beta
        val nextBeta = node.next match {
          case nextNode: Node[C] => nextNode.beta
          case _: Term[C] => ncols
        }
        if (nextBeta > minimalCorrectBefore) {
          @tailrec def populateRec(c: Int, rSubGrp1: Grp[R]): Unit =
            if (c < nextBeta) {
              val cmc = c <|+| minimalC
              cforRange(0 until nrows) { r =>
                minimal(c * nrows + r) = matrix(r <|+| minimalR, cmc)
              }
              val nextRSubGrp = symmetrySubgroup(rSubGrp1, minimalR, cmc)
              minimalSub(c) = nextRSubGrp
              populateRec(c + 1, nextRSubGrp)
            }
          populateRec(minimalCorrectBefore, rSubGrp)
          minimalCorrectBefore = nextBeta
        }
        node.foreachOrbit { b =>
          val bc = b <|+| cPerm
          val newCPerm = node.u(b) |+| cPerm
          // for the candidate to be here, finds columns that are minimal
          @tailrec def recCol(c: Int, rPerm1: R, rSubGrp1: Grp[R], isBetter: Boolean): Unit =
            if (c < nextBeta) {
              val ccp = c <|+| newCPerm
              val newRPerm1 = firstCol match {
                case Some(fun) if c == 0 => fun(r => matrix(r, ccp))
                case _ =>
                  val rLexChain1 = rSubGrp1.chain(rRep, BaseGuideLex(nrows))
                  val rSymGrp1 = symmetrySubgroup(rSubGrp1, rPerm1, ccp)
                  EnumAlgorithms.findMinimalPermutation(nrows, r => matrix(r <|+| rPerm1, ccp), rLexChain1, rSymGrp1, rRep) |+| rPerm1
              }
              val comp = compareToMinimal(ccp, newRPerm1, c)
              if (comp < 0 || isBetter) {
                cforRange(0 until nrows) { r =>
                  minimal(c * nrows + r) = matrix(r <|+| newRPerm1, ccp)
                }
                minimalSub(c) = symmetrySubgroup(rSubGrp1, newRPerm1, ccp)
              }
              if (comp <= 0 || isBetter)
                recCol(c + 1, newRPerm1, minimalSub(c), isBetter || comp < 0)
            } else {
              if (isBetter) {
                minimalCorrectBefore = nextBeta
                minimalR = rPerm1
                minimalC = newCPerm
                candidatesRPerm.clear
                candidatesCPerm.clear
              }
              candidatesRPerm += rPerm1
              candidatesCPerm += newCPerm
            }
          recCol(beta, rPerm, rSubGrp, false)
        }
        cforRange(0 until candidatesCPerm.length) { i =>
          val nextCPerm = candidatesCPerm(i)
          val ccp = beta <|+| nextCPerm
          val (nextCSymGrp, transversal) = cSymGrp.stabilizer(ccp, cRep)
          if (transversal.orbitMin == ccp) {
            val nextRPerm = candidatesRPerm(i)
            val nextRSub = minimalSub(nextBeta - 1)
            rec(level + 1, toLevel, nextRPerm, nextCPerm, nextRSub, node.next, nextCSymGrp)
          }
        }
      case _ =>
    }
    val cLexChain = cGrp.chain(cRep, BaseGuideLex(ncols)) match {
      case node: Node[C] if node.beta == 0 => node
      case other => TrivialNode(0, cFiniteGroup.id, other)
    }
    val cols = (0 until ncols).map(c => Seq.tabulate(nrows)(r => matrix(r, c)))
    val cSym = cGrp.fixingPartition(Domain.Partition.fromSeq(cols), cRep)
    cforRange(0 until cLexChain.length) { i =>
      rec(0, i, rFiniteGroup.id, cFiniteGroup.id, rGrp, cLexChain, cSym)
    }
    (minimalR, minimalC)
  }
}
*/