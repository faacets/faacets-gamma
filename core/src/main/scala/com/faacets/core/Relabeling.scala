package com.faacets.core

import spire.algebra._
import spire.syntax.cfor._
import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.perms._
import net.alasc.syntax.all._

import com.faacets.core.perm._
import com.faacets.data.{NiceGenerators, Textable}

abstract class Relabeling {
  def pPerm: Perm
  def xPerm(p: Int): Perm
  def aPerm(p: Int, x: Int): Perm

  /** Minimum number of parties in the scenario relabeled by this relabeling. */
  def nParties = nPartiesWithInputOutputRelabelings.max(nPartiesRelabeled)
  /** Size of the block of parties relabeled by the party relabeling `pPerm` */
  def nPartiesRelabeled: Int = pPerm.largestMovedPoint.getOrElseFast(0) + 1
  /** Size of the block of parties with relabeled inputs. */
  def nPartiesWithInputRelabelings: Int
  /** Size of the block of parties with relabeled outputs. */
  def nPartiesWithOutputRelabelings: Int
  /** Size of the block of parties with relabeled inputs/outputs (e.g. `PartyRelabeling`) */
  def nPartiesWithInputOutputRelabelings: Int
  /** Returns the number of inputs with relabelled outputs for the p-th party, or 0 if the `p`-th party
    * is not relabeled. */
  def nInputsWithOutputRelabelings(p: Int): Int
  def nInputs(p: Int): Int = spire.math.max(nInputsRelabeled(p), nInputsWithOutputRelabelings(p))
  def nInputsMax: Int = {
    var n = 0
    cforRange(0 until nPartiesWithInputOutputRelabelings) { p =>
      n = spire.math.max(n, nInputs(p))
    }
    n
  }
  def nInputsRelabeled(p: Int): Int = xPerm(p).largestMovedPoint.getOrElseFast(-1) + 1
  def nOutputs(p: Int, x: Int): Int = aPerm(p, x).largestMovedPoint.getOrElseFast(-1) + 1
  def nOutputsMax: Int = {
    var n = 0
    cforRange(0 until nPartiesWithOutputRelabelings) { p =>
      cforRange(0 until nInputsWithOutputRelabelings(p)) { x =>
        n = spire.math.max(n, nOutputs(p, x))
      }
    }
    n
  }

  /** In the compact notation of this `Relabeling`, number of relabelings of parties (0 or 1) */
  def numberOfPartyComponents =
    if (pPerm.isId) 0 else 1
  /** In the compact notation of this `Relabeling`, number of relabelings of inputs */
  def numberOfInputComponents = {
    var n = 0
    cforRange(0 until nPartiesWithInputOutputRelabelings) { p =>
      if (!xPerm(p).isId) n += 1
    }
    n
  }
  /** In the compact notation of this `Relabeling`, number of relabeling of outputs */
  def numberOfOutputComponents = {
    var n = 0
    cforRange(0 until nPartiesWithInputOutputRelabelings) { p =>
      cforRange(0 until nInputsWithOutputRelabelings(p)) { x =>
        if (!aPerm(p, x).isId) n += 1
      }
    }
    n
  }
  /** Total number of components in the compact notation of this `Relabeling` */
  def numberOfComponents = numberOfPartyComponents + numberOfInputComponents + numberOfOutputComponents

  /** Returns the party relabeling for the `p`-th party */
  def partyRelabeling(p: Int): PartyRelabeling
  /** Returns a map containing the party relabelings, when != identity */
  def partyRelabelingMap: Map[Int, PartyRelabeling] =
    (0 until nPartiesWithInputOutputRelabelings).map(p => (p, partyRelabeling(p)) ).filterNot(_._2.isId).toMap

  /** Returns the output relabelings in this `Relabeling` as a sequence of `Component` */
  def outputComponents: Seq[Relabeling.OutputComponent] =
    (0 until nPartiesWithInputOutputRelabelings).flatMap(p => partyRelabeling(p).outputComponents.map(_.forParty(p)) )
  /** Returns the input relabelings in this `Relabeling` as a sequence of `Component` */
  def inputComponents: Seq[Relabeling.InputComponent] =
    (0 until nPartiesWithInputOutputRelabelings).flatMap(p => partyRelabeling(p).inputComponents.map(_.forParty(p)) )
  /** Returns the party relabelings in this `Relabeling` as a sequence of `Component` */
  def partyComponents: Seq[Relabeling.PartyComponent] =
    if (pPerm.isId) Seq.empty else Seq(Relabeling.PartyComponent(pPerm))
  /** Returns the sequence of `Component` contained in this `Relabeling` */
  def components: Seq[Relabeling.Component] = outputComponents ++ inputComponents ++ partyComponents
  override def toString = components.mkString(" ")

  /** Returns the party relabeling part of this `Relabeling` */
  def partyPart: Relabeling
  /** Returns the input relabeling part of this `Relabeling` */
  def inputPart: Relabeling
  /** Returns the output relabeling part of this `Relabeling` */
  def outputPart: Relabeling
  /** Returns the output and input relabeling part of this `Relabeling` */
  def outputInputPart: Relabeling

  /** Returns the hash code for the relabeling.
    *
    * Relabelings components are hashed in a sequence using `MurmurHash3`, using `mix` and `finalizeHash`
    * (`mixLast` is not used). The sequence of components is ordered first by type (output components, then
    * input components, then party components), by party and finally by input. Components with an identity
    * permutation are not mixed in.
    *
    * Output components are mixed first using `(p << 16) + x` and then `a.hashCode`, where `p`, `x` are the
    * party and input index, and `a` is the permutation.
    * Input components are mixed first using `p`, then `x.hashCode`, where `x` is the input permutation.
    * Party components are mixed in using only `p.hashCode`, where `p` is the party permutation.
    *
    * The hash is finalized using the number of (non-identity) components.
    */
  override def hashCode = {
    import scala.util.hashing.MurmurHash3
    var hash = Relabeling.seed
    var n = 0
    cforRange(0 until nPartiesWithInputOutputRelabelings) { p =>
      cforRange(0 until nInputsWithOutputRelabelings(p)) { x =>
        val el = aPerm(p, x)
        if (!el.isId) {
          hash = MurmurHash3.mix(hash, (p << 16) + x)
          hash = MurmurHash3.mix(hash, el.hashCode)
          n += 1
        }
      }
    }
    cforRange(0 until nPartiesWithInputOutputRelabelings) { p =>
      val el = xPerm(p)
      if (!el.isId) {
        hash = MurmurHash3.mix(hash, p)
        hash = MurmurHash3.mix(hash, el.hashCode)
        n += 1
      }
    }
    if (!pPerm.isId) {
      hash = MurmurHash3.mix(hash, pPerm.hashCode)
      n += 1
    }
    MurmurHash3.finalizeHash(hash, n)
  }

  override def equals(other: Any) = other match {
    case that: Relabeling => Relabeling.equ.eqv(this, that)
    case _ => false
  }
}

trait RelabelingCompanion {
  def apply(prMap: Map[Int, PartyRelabeling], pPerm: Perm): Relabeling
  def apply(prSeq: Seq[PartyRelabeling], pPerm: Perm): Relabeling =
    apply(prSeq.zipWithIndex.map(_.swap).filterNot(_._2.isId).toMap, pPerm)
}

object Relabeling extends RelabelingCompanion {
  def id = group.empty
  val seed = "Relabeling".hashCode
  abstract class Component {
    def get: Relabeling
  }
  abstract class ComponentForParty extends Component {
    def p: Int
    def partyRelabeling: PartyRelabeling
    def get = partyRelabeling.forParty(p)
  }
  case class OutputComponent(p: Int, x: Int, a: Perm) extends ComponentForParty {
    override def toString = Party.prefixes(p) + x.toString + Cycles.fromPerm(a).string
    def partyRelabeling = PartyRelabeling.OutputComponent(x, a).get
  }
  case class InputComponent(p: Int, x: Perm) extends ComponentForParty {
    override def toString = Party.prefixes(p) + Cycles.fromPerm(x).string
    def partyRelabeling = PartyRelabeling.InputComponent(x).get
  }
  case class PartyComponent(p: Perm) extends Component {
    override def toString = Cycles.fromPerm(p).stringUsing(Party.prefixes(_))
    def get = Relabeling(Map.empty[Int, PartyRelabeling], p)
  }

  implicit val group: Group[Relabeling] = new RelabelingGroup

  implicit val equ: Eq[Relabeling] = new RelabelingEq

  implicit val imprimitiveImprimitiveRelabelingRepBuilder: FaithfulPermutationActionBuilder[Relabeling] =
    new ImprimitiveImprimitiveRelabelingRepBuilder

  implicit val tripletAction: Action[(Symbol, Int, Int), Relabeling] = new RelabelingTripletAction

  implicit val textable: Textable[Relabeling] = Textable.fromParser[Relabeling](perm.Parsers.relabeling, _.toString)

  def unapply(r: Relabeling): Option[(Map[Int, PartyRelabeling], Perm)] =
    Some((r.partyRelabelingMap, r.pPerm))

  def apply(prMap: Map[Int, PartyRelabeling], pPerm: Perm): Relabeling = {
    val nP = pPerm.largestMovedPoint.getOrElse(-1) + 1
    val nX = (1 /: prMap.valuesIterator) { case (mx, pr) => mx.max(pr.nInputsRelabeled) }
    val nA = (2 /: prMap.valuesIterator) { case (mx, pr) => mx.max(pr.nOutputsRelabeledMax) }
    if (nP <= 16 && nX <= 16 && nA <= 16)
      RelabelingImpl16(prMap, pPerm)
    else
      RelabelingImplGen(prMap, pPerm)
  }

  implicit val niceGenerators: NiceGenerators[Relabeling] = new NiceGenerators[Relabeling] {
    def niceGenerators(grp: Grp[Relabeling]) = RelabelingSubgroups(grp).niceGenerators
  }

}
