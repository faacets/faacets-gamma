package com.faacets
package operation
package relabeling

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.Group
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.group._
import spire.util._
import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{BaseGuideLex, Chain, GrpChainPermutationAction, Node, Term, TrivialNode}
import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.perms.default._
import net.alasc.perms.orbits.RepresentativesArrayInt
import net.alasc.syntax.group._

object MatrixAlgorithmsMaybeBroken {
  /** Returns the row and columns permutations that brings a matrix to its minimal lexicographic representatives, when the matrix is enumerated in
    * row-major order.
    * 
    * @param nrows    Number of rows in the matrix
    * @param ncols    Number of columns in the matrix
    * @param matrix   Function that returns the element (r,c) of the matrix as an (ordered) integer >= 0
    * @param rGrp     Group whose action permutes the rows
    * @param cGrp     Group whose action permutes the columns
    * @param rAction  Action of the group `rGrp` on the rows of the matrix
    * @param cAction  Action of the group `cGrp` on the columns of the matrix
    * @param firstCol Optional fast function to compute the minimal representative of a column under the full group rGrp.
    * 
    * @return permutations (rPerm, cPerm) such that matrix'(r, c) = matrix(r <|+| rPerm, cPerm <|+|) is lexicographically minimal when
    *         enumerated in row-major order
    */
  def findMinimalPermutation[R:ClassTag:GrpChainPermutationAction, C:ClassTag:GrpChainPermutationAction](nrows: Int, ncols: Int, matrix: (Int, Int) => Int,
                                   rGrp: Grp[R], cGrp: Grp[C],
                                   rAction: PermutationAction[R], cAction: PermutationAction[C], firstCol: Option[(Int => Int) => R] = None): (R, C) = {
    implicit def rGroup: Group[R] = rGrp.group
    implicit def cGroup: Group[C] = cGrp.group
    implicit def rActionImplicit: rAction.type = rAction
    implicit def cActionImplicit: cAction.type = cAction

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
    var minimalR = Group[R].id
    var minimalC = Group[C].id
    // compares the column c1, with its rows permuted by rPerm1 to the minimal column stored at minimalC2 (convention: permutedseq(i) = seq(i <|+| perm))
    def compareToMinimal(c1: Int, rPerm1: R, minimalC2: Int): Int = {
      cforRange(0 until nrows) { r =>
        val c = matrix(r <|+| rPerm1, c1) - minimal(r + minimalC2 * nrows)
        if (c != 0) return c
      }
      0
    }
    // returns the intersection fo grp with the symmetry subgroup of the c-th column permuted by rPerm (convention: permutedseq(i) = seq(i <|+| perm))
    def symmetrySubgroup(grp: Grp[R], rPerm: R, c: Int) =
      GrpChainPermutationAction[R].fixingPartition(grp, rAction, Partition.fromSeq(Seq.tabulate(nrows)(r => matrix(r <|+| rPerm, c))))
    // recurses column by column to find the best minimal representative
    def rec(level: Int, toLevel: Int, rPerm: R, cPerm: C, rSubGrp: Grp[R], cChain: Chain[C, cAction.type], cSymGrp: Grp[C]): Unit = cChain match {
      case node: Node[C, cAction.type] if level <= toLevel =>
        val candidatesRPerm = metal.mutable.Buffer.empty[R]
        val candidatesCPerm = metal.mutable.Buffer.empty[C]
        val beta = node.beta
        val nextBeta = node.next match {
          case nextNode: Node[C, cAction.type] => nextNode.beta
          case _: Term[C, cAction.type] => ncols
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
          // val bc = b <|+| cPerm not used?
          val newCPerm = node.u(b) |+| cPerm
          // for the candidate to be here, finds columns that are minimal
          @tailrec def recCol(c: Int, rPerm1: R, rSubGrp1: Grp[R], isBetter: Boolean): Unit =
            if (c < nextBeta) {
              val ccp = c <|+| newCPerm
              val newRPerm1 = firstCol match {
                case Some(fun) if c == 0 => fun(r => matrix(r, ccp))
                case _ =>
                  val rLexChain1 = GrpChainPermutationAction[R].fromGrp(rSubGrp1, rAction, Opt(BaseGuideLex(nrows)))
                  val rSymGrp1 = symmetrySubgroup(rSubGrp1, rPerm1, ccp)
                  RepresentativesArrayInt.findPermutationToMinimal(Array.tabulate(nrows)(r => matrix(r <|+| rPerm1, ccp)), rLexChain1, rSymGrp1).inverse |+| rPerm1 // or inverse of RepArrayInt ??? TODO
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
          val (nextCSymGrp, transversal) = GrpChainPermutationAction[C].stabilizerTransversal(cSymGrp, cAction, ccp)
          if (transversal.orbitMin == ccp) {
            val nextRPerm = candidatesRPerm(i)
            val nextRSub = minimalSub(nextBeta - 1)
            rec(level + 1, toLevel, nextRPerm, nextCPerm, nextRSub, node.next, nextCSymGrp)
          }
        }
      case _ =>
    }
    val cLexChain = GrpChainPermutationAction[C].fromGrp(cGrp, cAction, Opt(BaseGuideLex(ncols))).chain match {
      case node: Node[C, cAction.type] if node.beta == 0 => node
      case other => TrivialNode[C, cAction.type](0, other)
    }
    val cols = (0 until ncols).map(c => Seq.tabulate(nrows)(r => matrix(r, c)))
    val cSym = cGrp.fixingPartition(cAction, Partition.fromSeq(cols))
    cforRange(0 until cLexChain.length) { i =>
      rec(0, i, Group[R].id, Group[C].id, rGrp, cLexChain, cSym)
    }
    (minimalR, minimalC)
  }

}
