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