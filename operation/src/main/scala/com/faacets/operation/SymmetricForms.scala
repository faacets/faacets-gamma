package com.faacets
package operation

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

import spire.algebra.partial.PartialAction
import spire.syntax.action._
import spire.syntax.eq._
import spire.syntax.group._
import spire.util.Opt
import net.alasc.bsgs.{GrpChain, GrpChainPermutationAction, Node}
import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.perms.default._
import net.alasc.syntax.permutationAction._

import com.faacets.core._
import com.faacets.core.perm.ShapeLattice

// notes: http://groupprops.subwiki.org/wiki/Coset_intersection_problem
// http://groupprops.subwiki.org/wiki/Double_coset_membership_testing_problem
trait SymmetricForms[A] {

  implicit def action: PartialAction[A, Relabeling]

  def symmetryGroup(a: A): Grp[Relabeling]

  def symmetricRepresentative(a: A): ExtractedOperation[A, Relabeling] =
    ExtractedOperation(a, SymmetricForms.findSymmetricForm(symmetryGroup(a)))

  def cyclicRepresentative(a: A): ExtractedOperation[A, Relabeling] =
    ExtractedOperation(a, SymmetricForms.findCyclicForm(symmetryGroup(a)))

}

object SymmetricForms {

  implicit val exprSymmetricForms: SymmetricForms[Expr] = new SymmetricForms[Expr] {

    def action = instances.relabeling.operationExprRelabelingPartialAction

    def symmetryGroup(a: Expr) = a.symmetryGroup

  }

  def findSymmetricForm(symGrp: Grp[Relabeling]): Relabeling = {
    val nParties = symGrp.generators.map(_.nParties).fold(1)(_ max _)
    val remaining = mutable.BitSet(0 until nParties: _*)
    var r = Relabeling.id
    var newSym = false
    var curSymGrp = symGrp
    while (remaining.nonEmpty) {
      val p1 = remaining.head
      remaining -= p1
      val current = remaining.clone
      while (current.nonEmpty) {
        val p2 = current.head
        findElement(curSymGrp, p1, p2) match {
          case Some(member) =>
            val newR = Relabeling(Map(p2 -> member.partyRelabeling(p1).inverse), Perm.id)
            r = r |+| newR
            curSymGrp = curSymGrp.conjugatedBy(newR)
            remaining -= p2
            newSym = true
          case None =>
        }
        current -= p2
      }
    }
    if (newSym) r else Relabeling.id
  }

  def findCyclicForm(symGrp: Grp[Relabeling]): Relabeling = {
    // if there is local party relabeling such that
    // v <|+| Relabeling(Map(0 -> d, 1 -> e, 2 -> f), Perm(0,1,2))
    // then v is symmetric under relabeling by Relabeling(Map(0 -> d e^-1, 1 -> e f^-1, 2 -> f d^-1), Perm(0,1,2))
    // we find such relabeling using findCyclicElement
    findCyclicElement(symGrp, 0).fold(Relabeling.id) { rel =>
      // we then get the elements d e^-1, e f^-1, f d^-1, from which we can build
      // the elements d d^-1 = id, d e^-1, d f^-1, whose inverses can be used to build the
      // relabeling we are looking for
      @tailrec def partyRelabelings(p: Int, curMap: Map[Int, PartyRelabeling], cumPR: PartyRelabeling): Map[Int, PartyRelabeling] =
        if (p == 0) curMap else {
          val nextCumPR = cumPR |+| rel.partyRelabeling(p)
          partyRelabelings(p <|+| rel.pPerm, curMap + (p -> cumPR.inverse), nextCumPR)
        }
      Relabeling(partyRelabelings(0 <|+| rel.pPerm, Map.empty[Int, PartyRelabeling], rel.partyRelabeling(0)), Perm.id)
    }
  }

  /** Returns, if it exists, a member of `symGrp` (`symGrp` valid for `scenario`) of the form
    * `Relabeling(Map(p1 -> g, p2 -> g.inverse), Perm(p1, p2))`.
    * 
    * Note that this relabeling has order 2 (= self-inverse).
    */
  def findElement(symGrp: Grp[Relabeling], p1: Int, p2: Int): Option[Relabeling] = {
    val shapeLattice = ShapeLattice.fromRelabelings(symGrp.generators)
    val scenario = shapeLattice.scenario
    val action = shapeLattice.shape.ImpImpAction
    implicit def a: action.type = action
    if (scenario.parties(p1) =!= scenario.parties(p2)) return None // non-identical parties cannot be permuted
    val imp = shapeLattice.shape.imprimitiveImprimitive
    def pointsForParty(p: Int): Set[Int] = immutable.BitSet(imp.offsets(p) until imp.offsets(p + 1): _*)
    val pointsFixed = (scenario.parties.indices.toSet - p1 - p2).flatMap(pointsForParty(_))
    val p1p2Sym = symGrp.pointwiseStabilizer(action, pointsFixed)
    val length = imp.sizes(p1)
    val offset1 = imp.offsets(p1)
    val offset2 = imp.offsets(p2)
    def in1(k: Int) = k >= offset1 && k < offset1 + length
    def in2(k: Int) = k >= offset2 && k < offset2 + length
    def rec(curR: Relabeling, curGrp: GrpChain[Relabeling, action.type]): Option[Relabeling] =
      GrpChainPermutationAction[Relabeling].someStabilizerTransversal(curGrp, action) match {
        case Opt((nextGrp1, tr1)) =>
          // we are looking for a permutation that sends e.g.
          // (offset1 + i) to (offset2 + j) and (offset2 + j) to (offset1 + i)
          // thus, if we get e.g. beta = offset1 + i sent to br1 = offset2 + j
          // we take br1 = offset2 + j as the next base point in the stabilizer chain
          // possible redundancies are not removed by looking for a coset minimal representative,
          // as the coset order is always small
          val beta = tr1.beta
          if (in1(beta) || in2(beta)) {
            tr1.foreachOrbit { b1 =>
              val br1 = b1 <|+| curR
              if ( (in1(beta) && in2(br1)) || (in2(beta) && in1(br1)) ) {
                val nextR1 = tr1.u(b1) |+| curR
                val (nextGrp2, tr2) = GrpChainPermutationAction[Relabeling].stabilizerTransversal(nextGrp1, action, br1)
                val b2 = beta <|+| nextR1.inverse
                if (tr2.inOrbit(b2)) {
                  val nextR2 = tr2.u(b2) |+| nextR1
                  val res = rec(nextR2, nextGrp2)
                  if (res.nonEmpty) return res
                }
              }
            }
            None
          } else rec(curR, nextGrp1) // if there are some redundant points in the basis
        case _ =>
          val pr1 = curR.partyRelabeling(p1)
          val pr2 = curR.partyRelabeling(p2)
          if (pr1 === pr2.inverse) Some(curR) else None
      }
    rec(Relabeling.id, GrpChainPermutationAction[Relabeling].fromGrp(p1p2Sym, action))
  }

  def findCyclicElement(symGrp: Grp[Relabeling], startParty: Int): Option[Relabeling] = {
    val shapeLattice = ShapeLattice.fromRelabelings(symGrp.generators)
    val scenario = shapeLattice.scenario
    val action = shapeLattice.shape.ImpImpAction
    implicit def a: action.type = action
    val imp = shapeLattice.shape.imprimitiveImprimitive
    val length = imp.sizes(0)
    def offset(partyIndex: Int): Int = imp.offsets(partyIndex)
    def pointsForParty(p: Int): Iterable[Int] = imp.offsets(p) until imp.offsets(p + 1)
    def inParty(partyIndex: Int, k: Int): Boolean = {
      val partyOffset = offset(partyIndex)
      k >= partyOffset && k < partyOffset + length
    }
    def partyFor(k: Int): Int = imp.blockIndices(k)
    def isCorrect(rel: Relabeling): Boolean = {
      def recMul(p: Int, cumOp: PartyRelabeling): PartyRelabeling = {
        val pP = p <|+| rel.pPerm
        if (pP == startParty) cumOp else
          recMul(pP, cumOp |+| rel.partyRelabeling(pP))
      }
      val isCycleProductId = recMul(startParty, rel.partyRelabeling(startParty)).isEmpty
      val startPartyOrbit = rel.pPerm.orbit(startParty)
      val otherPartiesId = scenario.parties.indices.forall(p => rel.partyRelabeling(p).isEmpty || startPartyOrbit.contains(p))
      isCycleProductId && otherPartiesId
    }
    def inRec(loopTo: Int, curR: Relabeling, curGrp: Grp[Relabeling]): Option[Relabeling] =
      GrpChainPermutationAction[Relabeling].someStabilizerTransversal(curGrp, action) match {
      case Opt((nextGrp, tr)) =>
        tr.foreachOrbit { b =>
          val br = b <|+| curR
          val brParty = partyFor(br)
          if (brParty != startParty) {
            val nextR = tr.u(b) |+| curR
            val res = inRec(loopTo, nextR, nextGrp)
            if (res.nonEmpty) return res
          } else if (br == loopTo) {
            val nextR = tr.u(b) |+| curR
            val res = outRec(nextR, nextGrp)
            if (res.nonEmpty) return res
          }
        }
        None
      case _ => if (isCorrect(curR)) Some(curR) else None
    }
    def restart(beta: Int, curR: Relabeling, curGrp: Grp[Relabeling]): Option[Relabeling] = {
      val (nextGrp, tr) = GrpChainPermutationAction[Relabeling].stabilizerTransversal(curGrp, action, beta)
      tr.foreachOrbit { b =>
        val br = b <|+| curR
        if (!inParty(startParty, br)) {
          val nextR = tr.u(b) |+| curR
          val res = inRec(beta, nextR, nextGrp)
          if (res.nonEmpty) return res
        }
      }
      None
    }
    def outRec(curR: Relabeling, curGrp: Grp[Relabeling]): Option[Relabeling] = {
      GrpChainPermutationAction[Relabeling].fromGrp(curGrp, action).chain match {
        case node: Node[Relabeling, action.type] if inParty(startParty, node.beta) => restart(node.beta, curR, curGrp)
        case node: Node[Relabeling, action.type] =>
          pointsForParty(startParty).find(k => curGrp.generators.exists(g => k <|+| g != k)) match {
            case Some(beta) => restart(beta, curR, curGrp)
            case None => if (isCorrect(curR)) Some(curR) else None
          }
        case _ => if (isCorrect(curR)) Some(curR) else None
      }
    }
    outRec(Relabeling.id, symGrp)
  }

}
