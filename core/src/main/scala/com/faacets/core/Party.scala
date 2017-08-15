package com.faacets
package core

import com.faacets.core.perm._
import com.faacets.data.Textable
import net.alasc.finite.Grp
import net.alasc.util.Tuple2Int

/** Description of a party in a Bell scenario
  * 
  * This part defines a `Party` class which represents a single party in a Bell scenario.
  * The class contains:
  * - a textual representation using the bracket notation ( n_1 n_2 ... n_k ) where
  *   n_j is the number of outputs for the j-th input,
  * 
  * TODO
  * 
  * - definition of the finite group `finiteGroup` of outputs/inputs relabellings and
  *   two actions:
  *   - `probabilityAction` defines the action of the group on probability distributions
  *     \\( P(a|x) \\),
  *   - `strategyAction` defines the action of the group on strategy weights given as
  *     \\( q(\alpha_1, \alpha_2, ..., \alpha_k) \\), where \\( \alpha_j \\) is the
  *     deterministic output for the \\( j \\) -th input.
  */
final class Party private (val inputs: Seq[Int]) {
  assert(inputs.size >= 1)
  assert(inputs.forall(_ >= 1))

  def isHomogenous = inputs.forall(_ == inputs.head)

  def isCanonical =
    (inputs zip inputs.tail).forall { case (a, b) => a >= b }

  def nInputs = inputs.length

  def nOutputs(x: Int) = inputs(x)

  override def toString: String = inputs.mkString("(", " ", ")")

  override def hashCode = inputs.hashCode

  override def equals(any: Any) = any match {
    case that: Party => this eq that // uniqueness cache
    case _ => false
  }

  val shape = new PartyShape(inputs)

  val shapeLattice = PartyShapeLattice(inputs)

  lazy val matrices = repr.PartyMatrices(this)

  def shapeP: ImprimitiveShape = shape.imprimitive
  /** Decomposes an index in SP/NP representation into a tuple representing 
    * a 0-based output and input pair.
    */
  def ind2subP(ind: Int): Tuple2Int =
    Tuple2Int(shapeP.blockOffset(ind), shapeP.blockIndices(ind))
  def sub2indP(a: Int, x: Int): Int = shapeP.offsets(x) + a

  lazy val shapeNC: ImprimitiveShape = ImprimitiveShape(Array(1) ++ inputs.map(_ - 1))
  /** Decomposes an index in NC representation into a tuple representing
    * a 0-based group index and input pair.
    * 
    * When x == -1 and k == -1, the element is the marginal element.
    */
  def ind2subNC(ind: Int): Tuple2Int = {
    val k = shapeNC.blockOffset(ind)
    val x = shapeNC.blockIndices(ind) - 1
    if (x == -1) Tuple2Int(-1, -1) else Tuple2Int(k, x)
  }
  def sub2indNC(k: Int, x: Int): Int =
    shapeNC.offsets(x + 1) + (if (x == -1) 0 else k)

  /** Decomposes an index in NG representation into a tuple representing
    * a 0-based output and input pair.
    * 
    * When x == -1 and k == -1, the element is the marginal element.
    */
  def shapeNG: ImprimitiveShape = shapeNC
  def ind2subNG(ind: Int): Tuple2Int = ind2subNC(ind)
  def sub2indNG(k: Int, x: Int): Int = sub2indNC(k, x)

  lazy val shapeSC: ImprimitiveShape = ImprimitiveShape(Array(1) ++ inputs.map(_ - 1) :+ (inputs.length - 1))
  /** Decomposed an index in SG representation in a tuple representing
    * a 0-based group index and input pair.
    * 
    * When x == -1 and k == -1, the element is the marginal element.
    * When x == -1 and k >= 0, the element is an input element.
    */

  def ind2subSC(ind: Int): Tuple2Int = {
    val k = shapeSC.blockOffset(ind)
    val x = shapeSC.blockIndices(ind) - 1
    if (x == -1) Tuple2Int(-1, -1) // marginal element
    else if (x >= inputs.size) Tuple2Int(k, -1) else Tuple2Int(k, x)
  }
  def sub2indSC(k: Int, x: Int): Int =
    if (x == -1 && k == -1) 0
    else if (x == -1) shapeSC.offsets(inputs.size + 1) + k
    else shapeSC.offsets(x + 1) + k

  def shapeSG: ImprimitiveShape = shapeSC
  /** Decomposed an index in SG representation in a tuple representing
    * a 0-based output and input pair.
    * 
    * When x == -1 and k == -1, the element is the marginal element.
    * When x == -1 and k >= 0, the element is an input element.
    */
  def ind2subSG(ind: Int): Tuple2Int = ind2subSC(ind)
  def sub2indSG(k: Int, x: Int): Int = sub2indSC(k, x)

  def shapeW: PrimitiveShape = shape.primitive
  def ind2subW(ind: Int, array: Array[Int]): Unit = shapeW.ind2sub(ind, array)
  def ind2subW(ind: Int): Array[Int] = {
    val array = new Array[Int](inputs.size)
    ind2subW(ind, array)
    array
  }
  def sub2indW(sub: Array[Int]): Int = shapeW.sub2ind(sub)

  def shapeT: PrimitiveShape = shapeW
  def ind2subT(ind: Int, array: Array[Int]): Unit = ind2subW(ind, array)
  def ind2subT(ind: Int): Array[Int] = ind2subW(ind)
  def sub2indT(sub: Array[Int]): Int = sub2indW(sub)

  lazy val probabilityAction = shape.ImprimitiveAction

  lazy val strategyAction = shape.PrimitiveAction

  /** Object representing the symmetry group of one party of a Bell scenario.
    *
    * It is actually a inhomogenous wreath product group, composed of a
    * copy of S_(d_i) for each input i = 0 .. k-1, where d_i is the number
    * of outputs for the input k, and a subgroup of S_k permuting inputs
    * with the same number of outputs.
    */
  lazy val group: Grp[PartyRelabeling] =
    GrpLexAnsatz.fromGeneratorsAndOrder(subgroups.generators, subgroups.order, probabilityAction)

  lazy val subgroups = PartySubgroups(this)

  lazy val strategySubgroups = PartySubgroups(this, false)

}

/** Companion object for `Party`
  *
  * `Party` instances, when created, are kept in a `UniquenessCache` such that two parties 
  * with the same structure are represented by the same object in memory.
  */
object Party extends UniquenessCacheEq[Seq[Int], Party] {

  // UniquenessCache
  protected def valueFromKey(inputs: Seq[Int]): Party = new Party(inputs)
  protected def keyFromValue(party: Party): Option[Seq[Int]] = Some(party.inputs)

  val binaryIO: Party = Party.mk(2, 2)

 // Factory methods
  def mk(m: Int, k: Int): Party = Party(Seq.fill(m)(k))
  def prefix(p: Int): String = ('A' + p).toChar.toString
  val prefixes = (0 until 26).map(prefix)

  implicit val textable: Textable[Party] = Textable.fromParser[Party](Parsers.party, _.toString)

}
