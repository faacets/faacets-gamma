package com.faacets
package core

import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import spire.math.Rational

import cats.data.{Validated, ValidatedNel}
import com.faacets.consolidate.{Merge, Result}
import com.faacets.consolidate.syntax.all._

import net.alasc.perms.default._
import scalin.immutable.Vec

import net.alasc.attributes.{Attributable, Attributes}
import net.alasc.domains.Partition
import net.alasc.finite.Grp
import com.faacets.data.instances.textable._
import com.faacets.data.instances.vec._
import com.faacets.data.instances.grp._
import com.faacets.consolidate.instances.all._
import io.circe.Encoder

import net.alasc.bsgs.FixingPartition

/** Base class for vectors in the probability space of a causal scenario.
  *
  * A `PVec` is defined by:
  *
  * - a `scenario`,
  * - coefficients `coefficients` of a vector in the space or the dual space corresponding
  *   to linear forms.
  *
  * Probability distributions in a `Scenario` are described by `Behavior` objects.
  * Linear functionals on correlations and decompositions are the dual of these and are called `Expr`.
  *
  * Expressions which are not written in the canonical basis of the causal (nonsignaling) subspace are of class `DExpr`.
  *
  */
abstract class PVec extends Attributable { lhs =>

  def prefix: String

  override def toString = s"$prefix($scenario, $coefficients)"

  type S <: Scenario with Singleton

  val scenario: S

  type V <: PVec

  implicit def classTagV: ClassTag[V]

  def coefficients: Vec[Rational]

  override def hashCode: Int = scenario.hashCode + coefficients.hashCode

  override def equals(any: Any) = classTagV.unapply(any) match {
    case Some(that) => this === that
    case None => false
  }

  def ===(rhs: PVec): Boolean = ((lhs.scenario: Scenario) === (rhs.scenario: Scenario)) && (lhs.coefficients == rhs.coefficients) // TODO use Eq

  def coefficient(aArray: Array[Int], xArray: Array[Int]): Rational = {
    var ind = 0
    cforRange(0 until scenario.nParties) { p =>
      val partySub = scenario.parties(p).shapeP.offsets(xArray(p)) + aArray(p)
      ind = ind + scenario.shapeP.factors(p) * partySub
    }
    coefficients(ind)
  }

}

trait PVecEq[V <: PVec] extends Eq[V] {

  def eqv(lhs: V, rhs: V): Boolean = ((lhs.scenario:Scenario) === (rhs.scenario:Scenario)) && (lhs.coefficients == rhs.coefficients) // TODO Eq[Vec[Rational]]

}

abstract class NDVec extends PVec {

  def symmetryGroup: Grp[Relabeling] = NDVec.attributes.symmetryGroup(this) {
    val partition = Partition.fromSeq(coefficients.toIndexedSeq)
    scenario.group.fixingPartition(scenario.probabilityAction, partition)
  }

}

object NDVecBuilder {

  type ExprAux[S0 <: Scenario with Singleton] = Expr { type S = S0 }

  type BehaviorAux[S0 <: Scenario with Singleton] = Behavior { type S = S0 }

}

trait NDVecBuilder[V <: NDVec, VS[X <: Scenario with Singleton] <: V with NDVec.Aux[X]] {

  def apply(scenario: Scenario, coefficients: Vec[Rational]): VS[scenario.type]

  def inNonSignalingSubspace(scenario: Scenario, coefficients: Vec[Rational]): Boolean

  def validateInScenario[S <: Scenario with Singleton](scenario: S): (Vec[Rational], Option[Grp[Relabeling]]) => ValidatedNel[String, VS[S]] = {
    (coefficients: Vec[Rational], symGroup: Option[Grp[Relabeling]]) =>
    val correctLength =scenario.shapeP.size
    val coeffLength = coefficients.length
    if (coeffLength != correctLength) Validated.invalidNel(s"Invalid coefficients length, is $coeffLength, should be $correctLength")
    else if (!inNonSignalingSubspace(scenario, coefficients)) Validated.invalidNel("Coefficients are not in the nonsignaling subspace")
    else {
      val res = apply(scenario: S, coefficients)
      symGroup match {
        case Some(grp) =>
          val partition = Partition.fromSeq(coefficients.toIndexedSeq)
          grp.generators.find(!FixingPartition.partitionInvariantUnder(partition, scenario.probabilityAction, _)) match {
            case Some(g) =>
              Validated.invalidNel(s"Coefficients are not invariant under provided generator $g")
            case None =>
              NDVec.attributes.symmetryGroup(res)(grp)
              Validated.valid(res)
          }
        case None =>
          Validated.valid(res)
      }
    }
  }

  def mergeInScenario(scenario: Scenario): Merge[VS[scenario.type]] = new Merge[VS[scenario.type]] {

    def merge(base: VS[scenario.type], newV: VS[scenario.type]): Result[VS[scenario.type]] = {
      import cats.syntax.all._
      import NDVec.attributes.{symmetryGroup => sg}
      val coefficients = base.coefficients merge newV.coefficients
      val symGroup = sg.get(base) merge sg.get(newV)
      (coefficients |@| symGroup).map((_,_)).validate(validateInScenario[scenario.type](scenario).tupled)
    }

  }

  implicit lazy val merge: Merge[V] = new Merge[V] {

    def merge(base: V, newV: V): Result[V] = {
      import cats.syntax.all._
      import NDVec.attributes.{symmetryGroup => sg}
      (base.scenario: Scenario) merge (newV.scenario: Scenario) match {
        case f: Result.Failed => f
        case _: Result.Updated[_] => sys.error("Cannot happen")
        case Result.Same(scenario) =>
          val coefficients = base.coefficients merge newV.coefficients
          val symGroup = sg.get(base) merge sg.get(newV)
          (coefficients |@| symGroup).map((_,_)).validate(validateInScenario[scenario.type](scenario).tupled)
      }
    }

  }

  import com.faacets.data.instances.textable._

  lazy val encodeWithGroup: Encoder[(V, Grp[Relabeling])] =
    Encoder.forProduct3[Scenario, Vec[Rational], Grp[Relabeling], (V, Grp[Relabeling])]("scenario", "coefficients", "symmetryGroup")( pair => (pair._1.scenario, pair._1.coefficients, pair._2) )
  lazy val encodeWithoutGroup: Encoder[V] =
    Encoder.forProduct2("scenario", "coefficients")( (v: V) => (v.scenario: Scenario, v.coefficients) )

  implicit val encode: Encoder[V] = Encoder.instance[V] { v =>
    NDVec.attributes.symmetryGroup.get(v) match {
      case Some(grp) => encodeWithGroup( (v, grp) )
      case None => encodeWithoutGroup(v)
    }
  }

}

object NDVec {

  type Aux[S0 <: Scenario with Singleton] = NDVec { type S = S0 }

  object attributes extends Attributes("NDVec") {

    object symmetryGroup extends Attribute("symmetryGroup") {
      implicit def forNDVec: For[NDVec, Grp[Relabeling]] = For
    }

  }

}
