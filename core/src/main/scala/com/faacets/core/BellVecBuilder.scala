package com.faacets
package core

/*
import scala.language.implicitConversions

import spire.algebra.VectorSpace
import spire.math.Rational
import spire.syntax.cfor._
import spire.util.Opt

import com.faacets.consolidate.Merge
import scalin.immutable.Vec
import scalin.syntax.all._
import scalin.immutable.dense._
import perm._
import data._
import data.any._
import consolidate._
import consolidate.syntax.all._
import consolidate.std.any._

import net.alasc.finite.Grp

abstract class BellVecBuilder[V[X <: Scenario with Singleton] <: PVec[V, X]] {

  implicit def builder: BellVecBuilder[V] = this

  def apply(scenario: Scenario, coefficients: Vec[Rational]): V[scenario.type]

  def zero(scenario: Scenario): V[scenario.type] = {
    import net.alasc.finite.Rep.convert._
    val res = apply(scenario, zeros[Rational](scenario.probabilityRep.dimension))
    res._attrUpdate(PVec.symmetryGroup, scenario.probabilityGroup: Grp[Relabeling])
    res
  }

  /*
  def one(scenario: Scenario, representation: Representation): V = {
    val simpleReprVec = representation match {
      case NPRepresentation | SPRepresentation |
           NGRepresentation | SGRepresentation |
           NCRepresentation | SCRepresentation =>
        single(scenario, NCRepresentation, 0)
      case TRepresentation | WRepresentation =>
        single(scenario, NCRepresentation, 0)
    }
    simpleReprVec.to(representation)
  }*/

  implicit def optFromOption[X](option: Option[X]): Opt[X] = option match {
    case Some(x) => Opt(x)
    case None => Opt.empty[X]
  }

  class BellVecMerge[S <: Scenario with Singleton] extends Merge[V[S]] {
    def merge(current: V[S], other: V[S]): Merged[V[S]] = for {
      coefficients <- (current.coefficients merge other.coefficients) withPath "coefficients"
      symmetryGroupOption <- (
        current.attr.get(PVec.symmetryGroup).toOption merge
        other.attr.get(PVec.symmetryGroup).toOption) withPath "symmetryGroup"
    } yield {
      val res = builder(current.scenario: S, coefficients)
      symmetryGroupOption.foreach { grp => res._attrUpdate(PVec.symmetryGroup, grp) }
      res
    }
  }

  implicit def merge[S <: Scenario with Singleton] = new BellVecMerge[S]

  implicit def equ[S <: Scenario with Singleton] = spire.optional.genericEq.generic[V[S]]

}
/*
abstract class VecBuilder[V <: Vec[V]] {

  implicit val Format: Format[V] = (
    (JsPath \ "scenario").format[Scenario] and
      (JsPath \ "representation").format[Representation] and
      (JsPath \ "coefficients").format[QVector]
  )(apply(_, _, _), v => (v.scenario, v.representation, v.coefficients))

  def buildNC(scenario: Scenario)(coefficients: (Array[Int], Array[Int]) => Rational): V = {
    val n = scenario.nParties
    val subArray = new Array[Int](n)
    val kArray = new Array[Int](n)
    val xArray = new Array[Int](n)
    val coeffVector = QVector.tabulate(scenario.shapeNC.size) { ind =>
      scenario.shapeNC.ind2sub(ind, subArray)
      cforRange(0 until n) { p =>
        val sub = subArray(p)
        val partyShape = scenario.parties(p).shapeNC
        kArray(p) = partyShape.blockOffset(sub)
        xArray(p) = partyShape.blockIndices(sub) - 1
      }
      coefficients(kArray, xArray)
    }
    apply(scenario, NCRepresentation, coeffVector)
  }

  def apply(scenario: Scenario, term: Term[Element]): V = {
    val (representation, index) = Term.find(scenario, term)
    single(scenario, representation, index)
  }

  implicit def logical(b: Boolean): Rational = if (b) Rational.one else Rational.zero

  protected def single(scenario: Scenario, representation: Representation, termIndex: Int) = {
    val d = scenario.primitiveShape(representation).size
    val coefficients = QVector.tabulate(d)(k => (termIndex == k): Rational)
    representation match {
      case TRepresentation | NCRepresentation | SCRepresentation if termIndex == 0 =>
        apply(scenario, representation, coefficients, Some(scenario.group))
      case _ =>
        apply(scenario, representation, coefficients, None)
    }
  }

  def parseExpression(text: String): V = 
    VecParser.parseAll(VecParser.vec[V], text) match {
      case success: VecParser.Success[V] => success.result
      case error => throw new IllegalArgumentException(error.toString)
    }

  def parseExpression(scenario: Scenario)(text: String): V = 
    VecParser.parseAll(VecParser.vec[V](scenario), text) match {
      case success: VecParser.Success[V] => success.result
      case error => throw new IllegalArgumentException(error.toString)
    }

}
*/
*/