package com.faacets
package core

import com.faacets.data.Textable
import spire.math.SafeLong
import spire.syntax.cfor._
import scalin.immutable.Vec
import net.alasc.finite.Grp
import net.alasc.perms.default._
import perm._
import consolidate.Merge
import com.faacets.data.syntax.textable._

/** TODO: verify doc below
  *
  * Description of a Bell scenario.
  * 
  * A `Scenario` object describes a Bell scenario composed of several parties,
  * each having a certain number of measurement settings, each setting having a number
  * of possible outcomes.
  * 
  * Instances of `Scenario` also enable:
  * 
  * - description of the different representations using `Repr` objects,
  * - conversion of `Expr` or `Corr` between representations,
  * - `Term` instances to represent the elements of a `Expr` or `Corr` in any representation,
  * - the construction of its Bell permutation group, some remarkable subgroups 
  and the following actions:
  * - `marginalAction` defines the action of the group on concatenated single party
  *    marginals \\( P(a|x) P(b|y) ... \\),
  * - `probabilityAction` defines the action of the group on probability distributions
  *    \\( P(a b ... | x y ...) \\),
  * - `strategyAction` defines the action of the group on strategy weights.
  * 
  * A `Scenario` can be represented in plain text using the following grammar:
  * - `Scenario := [Party Party ... ]`
  * - `Party := (Input Input ...)`
  * - `Input := number >= 1`
  * 
  * Examples:
  * 
  * - the CHSH scenario is written down [(2 2) (2 2)],
  * - the Sliwa scenario is written down [(2 2) (2 2) (2 2)],
  * - the I2233 scenario is written down [(3 3) (3 3)],
  * - the I3322 scenario is written down [(2 2 2) (2 2 2]].*/
final class Scenario private (val parties: Seq[Party]) {

  override def hashCode = parties.hashCode

  override def toString = parties.map(_.toText).mkString("[", " ", "]")

  override def equals(any: Any) = any match {
    case that: Scenario => this eq that
    case _ => false
  }

  def hasHomogenousParties = (1 until parties.length).forall(parties(_) == parties(0))
  /** The scenario can be represented using a Java identifier,
    * for example to name keys in the repository.*/
  def toIdentifier = parties.map(party => party.inputs.mkString("")).mkString("_")

  val minNumInputs = parties.map(_.inputs.length).min
  val maxNumInputs = parties.map(_.inputs.length).max
  val minNumOutputs = parties.flatMap(_.inputs).min
  val maxNumOutputs = parties.flatMap(_.inputs).max
  val nParties = parties.length

  /** Number of different input tuples, i.e. the number of different measurements overall.
    * Example: Scenario.CHSH.nInputTuples === 4. */
  val nInputTuples = parties.foldLeft(SafeLong(1)) { case (mul, party) => mul * SafeLong(party.nInputs) }

  val shape = new Shape(parties)
  val shapeLattice = ShapeLattice(parties)

  def tabulateP[A](coefficients: (Array[Int], Array[Int]) => A): Vec[A] = {
    import scalin.immutable.dense._
    val n = nParties
    val subArray = new Array[Int](n)
    val aArray = new Array[Int](n)
    val xArray = new Array[Int](n)
    Vec.tabulate(shapeP.size) { ind =>
      shapeP.ind2sub(ind, subArray)
      cforRange(0 until n) { p =>
        val partyShape = parties(p).shapeP
        val sub = subArray(p)
        aArray(p) = partyShape.blockOffset(sub)
        xArray(p) = partyShape.blockIndices(sub)
      }
      coefficients(aArray, xArray)
    }
  }

  def tabulateNG[A](coefficients: (Array[Int], Array[Int]) => A): Vec[A] =
    tabulateNC(coefficients)

  def tabulateNC[A](coefficients: (Array[Int], Array[Int]) => A): Vec[A] = {
    import scalin.immutable.dense._
    val subArray = new Array[Int](nParties)
    val kArray = new Array[Int](nParties)
    val xArray = new Array[Int](nParties)
    Vec.tabulate(shapeNC.size) { ind =>
      shapeNC.ind2sub(ind, subArray)
      cforRange(0 until nParties) { p =>
        val sub = subArray(p)
        val partyShape = parties(p).shapeNC
        kArray(p) = partyShape.blockOffset(sub)
        xArray(p) = partyShape.blockIndices(sub) - 1
      }
      coefficients(kArray, xArray)
    }
  }

  def shapeP: PrimitiveShape = shape.primitiveImprimitive

  def ind2subP(ind: Int, aArray: Array[Int], xArray: Array[Int]): Unit = {
    shapeP.ind2sub(ind, aArray) // recycle aArray
    cforRange(0 until nParties) { p =>
      val tupleAX = parties(p).ind2subP(aArray(p))
      aArray(p) = tupleAX._1
      xArray(p) = tupleAX._2
    }
  }

  def ind2subP(ind: Int): (Array[Int], Array[Int]) = {
    val aArray = new Array[Int](nParties)
    val xArray = new Array[Int](nParties)
    ind2subP(ind, aArray, xArray)
    (aArray, xArray)
  }

  def sub2indP(aArray: Array[Int], xArray: Array[Int]): Int = {
    var ind = 0
    cforRange(0 until nParties) { p =>
      val partySub = parties(p).sub2indP(aArray(p), xArray(p))
      ind = ind + shapeP.factors(p) * partySub
    }
    ind
  }

  lazy val shapeNC: PrimitiveShape = PrimitiveShape(parties.map(party => party.inputs.sum - party.inputs.size + 1).toArray)

  def ind2subNC(ind: Int, kArray: Array[Int], xArray: Array[Int]): Unit = {
    shapeNC.ind2sub(ind, kArray) // recycle kArray
    cforRange(0 until nParties) { p =>
      val  tupleKX = parties(p).ind2subNC(kArray(p))
      kArray(p) = tupleKX._1
      xArray(p) = tupleKX._2
    }
  }

  def ind2subNC(ind: Int): (Array[Int], Array[Int]) = {
    val kArray = new Array[Int](nParties)
    val xArray = new Array[Int](nParties)
    ind2subNC(ind, kArray, xArray)
    (kArray, xArray)
  }

  def sub2indNC(kArray: Array[Int], xArray: Array[Int]): Int = {
    var ind = 0
    cforRange(0 until nParties) { p =>
      val partySub = parties(p).sub2indNC(kArray(p), xArray(p))
      ind = ind + shapeNC.factors(p) * partySub
    }
    ind
  }

  def shapeNG: PrimitiveShape = shapeNC

  def ind2subNG(ind: Int, kArray: Array[Int], xArray: Array[Int]): Unit = ind2subNC(ind, kArray, xArray)

  def ind2subNG(ind: Int): (Array[Int], Array[Int]) = ind2subNC(ind)

  def sub2indNG(kArray: Array[Int], xArray: Array[Int]): Int = sub2indNC(kArray, xArray)

  def shapeSC: PrimitiveShape = shape.primitiveImprimitive

  def shapeSG: PrimitiveShape = shape.primitiveImprimitive

  def shapeT: PrimitiveShape = shape.primitivePrimitive

  def shapeW: PrimitiveShape = shape.primitivePrimitive

  def isCSignalingIndex(sub: Array[Int]): Boolean = {
    cforRange(0 until nParties) { p =>
      if (sub(p) > parties(p).shapeP.size - parties(p).nInputs)
        return true
    }
    false
  }

  def isCNormalizationIndex(sub: Array[Int]): Boolean = {
    var hasSignaling = false
    var hasNormalization = false
    var hasContent = false
    cforRange(0 until nParties) { p =>
      if (sub(p) == 0) hasNormalization = true
      else if (sub(p) <= parties(p).shapeP.size - parties(p).nInputs) hasContent = true
      else hasSignaling = true
    }
    !hasContent
  }

  lazy val marginalAction = shape.ImpImpAction

  lazy val probabilityAction = shape.PriImpAction

  lazy val strategyAction = shape.PriPriAction

  /** The value `group` represents the symmetry group of the current
    * Bell scenario. This group is actually a wreath product group,
    * composed of a copy of \\( n \\) party symmetry group and the subgroup of \\( S_n \\)
    * permuting parties that are compatible, i.e. with the same output structure. */
  lazy val group: Grp[Relabeling] =
    GrpLexAnsatz.fromGeneratorsAndOrder(subgroups.generators, subgroups.order, marginalAction)

  lazy val subgroups = ScenarioSubgroups(this)

  lazy val probabilitySubgroups = ScenarioSubgroups(this, permuteSingleInputOutputParties = false)

  lazy val strategySubgroups = ScenarioSubgroups(this, false, false)

}

/** Companion object for `Scenario`
  * 
  * As for `Party`, the instances of `Scenario`, when created, are kept in an
  * `UniquenessCache` such that two scenarios with the same structure are represented
  * by the same object in memory.
  */
object Scenario extends UniquenessCacheEq[Seq[Party], Scenario] {
/*  val remarkableSubgroupNames = Seq(
    "liftings", "outputPermsPerParty", "outputPerms",
    "inputPermsPerParty", "inputPerms",
    "outputInputPermsPerParty", "outputInputPerms",
    "partyPerms",
    "rest")*/

  protected def valueFromKey(parties: Seq[Party]): Scenario = new Scenario(parties)
  protected def keyFromValue(scenario: Scenario): Option[Seq[Party]] = Some(scenario.parties)

  // Factory methods
  def nmk(n: Int, m: Int, k: Int) = {
    val party = Party.mk(m, k)
    apply(Seq.fill(n)(party))
  }

  val CHSH = nmk(2, 2, 2)

  implicit val textable: Textable[Scenario] = Textable.fromParser[Scenario](Parsers.scenario, _.toString)

  implicit val merge: Merge[Scenario] = consolidate.Merge.fromEquals[Scenario]

}
