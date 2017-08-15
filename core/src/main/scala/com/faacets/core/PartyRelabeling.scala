package com.faacets.core

import spire.algebra._
import spire.syntax.cfor._
import com.faacets.core.perm._
import com.faacets.data.Textable
import net.alasc.finite.FaithfulPermutationActionBuilder
import net.alasc.perms.{Cycles, Perm}
import net.alasc.syntax.all._

/** Describes the relabeling of inputs and outputs of a party, independently of the size of the party. */
abstract class PartyRelabeling {

  def aPerm(x: Int): Perm
  def xPerm: Perm

  /** Returns the size of the block of relabeled inputs */
  def nInputsRelabeled: Int = xPerm.largestMovedPoint.getOrElseFast(0) + 1
  /** Returns the size of the block of inputs with output relabelings */
  def nInputsWithOutputRelabelings: Int
  /** Returns the number of relabeled outputs for the `x`-th output */
  def nOutputsRelabeled(x: Int): Int = aPerm(x).largestMovedPoint.getOrElseFast(-1) + 1
  /** Returns the maximum number of relabeled outputs in this `PartyRelabeling` */
  def nOutputsRelabeledMax: Int = {
    var mx = 0
    cforRange(0 until nInputsWithOutputRelabelings) { x =>
      mx = mx.max(nOutputsRelabeled(x))
    }
    mx
  }
  /** Returns the minimal size of the block of inputs for this `PartyRelabeling` */
  def nInputs = nInputsWithOutputRelabelings.max(nInputsRelabeled)

  /** Returns the sequence of input `Component` for this `PartyRelabeling` */
  def inputComponents: Seq[PartyRelabeling.InputComponent] =
    if (xPerm.isId) Seq.empty else Seq(PartyRelabeling.InputComponent(xPerm))
  /** Returns the sequence of output `Component` for this `PartyRelabeling` */
  def outputComponents =
    Seq.empty[PartyRelabeling.OutputComponent] ++ (
      for (x <- 0 until nInputsWithOutputRelabelings if !aPerm(x).isId) yield PartyRelabeling.OutputComponent(x, aPerm(x))
    )
  /** Returns a decomposition of this `PartyRelabeling` into a sequence of `Component` */
  def components: Seq[PartyRelabeling.Component] =
    outputComponents ++ inputComponents
  /** Returns a `String` representation of this `PartyRelabeling` */
  override def toString = components.mkString(" ")
  /** Returns a `Map` of output relabelings contained in this `PartyRelabeling` */
  def outputRelabelingMap: Map[Int, Perm] =
    (0 until nInputsWithOutputRelabelings).map( x => (x, aPerm(x)) ).filterNot(_._2.isId).toMap
  /** Returns the part of this `PartyRelabeling` that relabels outputs. */
  def outputPart: PartyRelabeling
  /** Returns the part of this `PartyRelabeling` that relabels inputs. */
  def inputPart: PartyRelabeling
  /** Returns an instance of a `Relabeling` when this `PartyRelabeling` acts on the `p`-th party */
  def forParty(p: Int) = Relabeling(Map(p -> this), Group[Perm].id)

  /** Returns the hash code for the party relabeling.
    * 
    * Party relabelings components are hashed in a sequence using `MurmurHash3`, using `mix` and `finalizeHash`
    * (`mixLast` is not used). The sequence of components is ordered first by type (output components, then
    * input components), and then by input. Components with an identity permutation are not mixed in.
    * 
    * Output components are mixed first using `x` and then `a.hashCode`, where `x` is the input index, 
    * and `a` is the permutation.
    * Input components are mixed using `x.hashCode`, where `x` is the input permutation.
    * 
    * The hash is finalized using the number of (non-identity) components.
    */
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3
    var hash = Relabeling.seed
    var n = 0
    cforRange(0 until nInputsWithOutputRelabelings) { x =>
      val el = aPerm(x)
      if (!el.isId) {
        hash = MurmurHash3.mix(hash, x)
        hash = MurmurHash3.mix(hash, el.hashCode)
        n += 1
      }
    }
    if (!xPerm.isId) {
      hash = MurmurHash3.mix(hash, xPerm.hashCode)
      n += 1
    }
    MurmurHash3.finalizeHash(hash, n)
  }

  override def equals(other: Any) = other match {
    case that: PartyRelabeling => PartyRelabeling.equ.eqv(this, that)
    case _ => false
  }

}

trait PartyRelabelingCompanion {

  def apply(aSeq: Seq[Perm], xPerm: Perm): PartyRelabeling =
    apply(aSeq.zipWithIndex.map(_.swap).filterNot(_._2.isId).toMap, xPerm)

  def apply(aMap: Map[Int, Perm], xPerm: Perm): PartyRelabeling

}

object PartyRelabeling extends PartyRelabelingCompanion {

  def id = group.empty

  abstract class Component {
    def get: PartyRelabeling
    def forParty(p: Int): Relabeling.Component
  }

  case class OutputComponent(x: Int, a: Perm) extends Component {
    override def toString = x.toString + Cycles.fromPerm(a).string
    def get = PartyRelabeling(Map(x -> a), Group[Perm].id)
    def forParty(p: Int) = Relabeling.OutputComponent(p, x, a)
  }

  case class InputComponent(x: Perm) extends Component {
    override def toString = "I" + Cycles.fromPerm(x).string
    def get = PartyRelabeling(Map.empty[Int, Perm], x)
    def forParty(p: Int) = Relabeling.InputComponent(p, x)
  }

  def apply(aMap: Map[Int, Perm], xPerm: Perm) = {
    val nX = xPerm.largestMovedPoint.getOrElse(-1) + 1
    val nA = (2 /: aMap.valuesIterator) { case  (mx, p) => mx.max(p.largestMovedPoint.getOrElse(-1) + 1) }
    if (nX <= 16 && nA <= 16)
      PartyRelabelingImpl16(aMap, xPerm)
    else
      PartyRelabelingImplGen(aMap, xPerm)
  }

  def unapply(pr: PartyRelabeling): Option[(Map[Int, Perm], Perm)] =
    Some((pr.outputRelabelingMap, pr.xPerm))

  implicit val group: Group[PartyRelabeling] = new PartyRelabelingGroup

  implicit val equ: Eq[PartyRelabeling] = new PartyRelabelingEq

  implicit val pairAction: Action[(Int, Int), PartyRelabeling] = new PartyRelabelingPairAction

  implicit val textable: Textable[PartyRelabeling] = Textable.fromParser[PartyRelabeling](perm.Parsers.partyRelabeling, _.toString)

  implicit lazy val imprimitivePartyRelabelingRepBuilder: FaithfulPermutationActionBuilder[PartyRelabeling] =
    new ImprimitivePartyRelabelingRepBuilder

}
