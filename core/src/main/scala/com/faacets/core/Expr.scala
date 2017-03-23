package com.faacets
package core

/*
import spire.math.{Rational, SafeLong}
import scala.reflect.classTag

import net.alasc.algebra._
import scalin.immutable.dense._
import scalin.immutable.Vec
import spire.syntax.cfor._

import scalin.syntax.all._
import com.faacets.core.repr.{NCRepresentation, NGRepresentation, Represented, SCRepresentation}

/** Describes a Bell expression. */
final class Expr[S <: Scenario with Singleton](val scenario: S, val coefficients: Vec[Rational]) extends BellVec[Expr, S] {

  override def toString = s"Expr($scenario, $coefficients)"

  def classTagV = classTag[Expr[S]]

  def builder = Expr

  def isInNonSignalingSubspace: Boolean = {
    val sub = new Array[Int](scenario.nParties)
    val scCoeffs = repr.SCRepresentation.fromExpr(this).coefficients
    cforRange(0 until scCoeffs.length) { ind =>
      scenario.shapeSC.ind2sub(ind, sub)
      if (scenario.isCSignalingIndex(sub) && !scCoeffs(ind).isZero)
        return false
    }
    true
  }

  /** Decomposes this expression in the non-signaling and its biorthogonal subspace.
    * A `Expr` in a signaling representation can be projected into the non-signaling subspace.
    * The method `toNonSignaling` can be used to that effect: it returns the non-signaling
    * projected `Expr` in a non-signaling representation, and a possibly signaling `Expr`
    * containing only the signaling and proper normalization terms.
    */
  def toNonSignalingSubspace: (Expr[S], Expr[S]) = {
    val sub = new Array[Int](scenario.nParties)
    val scCoeffs = SCRepresentation.fromExpr(this).coefficients
    val nsCoeffs = tabulate(scCoeffs.length) { ind =>
      scenario.shapeSC.ind2sub(ind, sub)
      if (scenario.isCSignalingIndex(sub)) Rational.zero else scCoeffs(ind)
    }
    val sCoeffs = scCoeffs - nsCoeffs
    (SCRepresentation.expr(scenario: S, nsCoeffs).value, SCRepresentation.expr(scenario: S, sCoeffs).value)
  }

  /** Removes signaling/proper normalization terms. */
  def removingSignalingTerms = toNonSignalingSubspace._1

  /*
    override def toString = {
      import pretty._
      import pretty.syntax.all._
      this.prettyExpression.pretty[String]
    }

    def constant = Vec.inner(this, Corr.one(scenario, representation))*/

}

object Expr extends BellVecBuilder[Expr] {

  def averageNormalization(scenario: Scenario): Expr[scenario.type] = {
    val coeff = Rational(1, scenario.nInputTuples)
    new Expr[scenario.type](scenario, scalin.syntax.build.tabulate(scenario.probabilityRep.dimension)(k => coeff))
  }

  def CHSH = NCRepresentation.expr(Scenario.CHSH,
    vec(0, 0, 0, 0, 1, 1, 0, 1, -1).to[Rational].get).value
  def Sliwa10 = NCRepresentation.expr(Scenario.nmk(3, 2, 2),
    vec(4, 0, 0, 0, -1, -1, 0, -1, -1, 0, -1, 1, -1, -1, 0, 1, 0, 1, 0, -1, 1, 1, 0, -1, -1, 1, 0).to[Rational].get).value
  def Sliwa10Sym = NCRepresentation.expr(Scenario.nmk(3, 2, 2),
    vec(4, 0, 0, 0, -1, -1, 0, 1, 1, 0, -1, 1, -1, -1, 0, -1, 0, -1, 0, -1, 1, 1, 0, -1, 1, -1, 0).to[Rational].get).value
  def Sliwa7 = NCRepresentation.expr(Scenario.nmk(3, 2, 2),
    vec(4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -3, -1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1).to[Rational].get).value
  def Sliwa4 = NCRepresentation.expr(Scenario.nmk(3, 2, 2),
    vec(2, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1, 0).to[Rational].get).value
  def I3322 = NGRepresentation.expr(Scenario.nmk(2, 3, 2),
    vec(0, -1, 0, 0, -2, 1, 1, 1, -1, 1, 1, -1, 0, 1, -1, 0).to[Rational].get).value

  def apply(scenario: Scenario, coefficients: Vec[Rational]): Expr[scenario.type] =
    new Expr[scenario.type](scenario, coefficients)

}
*/