package com.faacets
package core

/*
import spire.algebra.VectorSpace
import spire.math.Rational
import spire.syntax.all._
import spire.util.Opt

import com.faacets.core.perm.Relabeling
import scalin.syntax.all._
import scalin.immutable.dense._

import net.alasc.finite.Grp
import net.alasc.finite.Rep.convert._

trait BellVecVectorSpace[V[X <: Scenario with Singleton] <: BellVec[V, X], S <: Scenario with Singleton]
  extends Any with VectorSpace[V[S], Rational] {

  def scenario: S

  implicit def builder: BellVecBuilder[V]

  implicit def scalar = spire.math.Rational.RationalAlgebra

  def negate(v: V[S]): V[S] = {
    val res = builder(scenario, -v.coefficients)
    v.attr.get(BellVec.symmetryGroup) match {
      case Opt(grp) => res._attrUpdate(BellVec.symmetryGroup, grp)
      case _ =>
    }
    res
  }

  def zero: V[S] = {
    val res = builder(scenario, zeros[Rational](scenario.probabilityRep.dimension))
    res._attrUpdate(BellVec.symmetryGroup, scenario.probabilityGroup: Grp[Relabeling])
    res
  }

  def timesl(lhs: Rational, rhs: V[S]): V[S] =
    if (lhs != 0) {
      val res = builder(scenario, rhs.coefficients * lhs)
      res.attr.get(BellVec.symmetryGroup) match {
        case Opt(grp) => res._attrUpdate(BellVec.symmetryGroup, grp)
      }
      res
    } else zero

  def plus(lhs: V[S], rhs: V[S]): V[S] =
    builder(scenario, lhs.coefficients + rhs.coefficients)

  override def minus(lhs: V[S], rhs: V[S]): V[S] =
    builder(scenario, lhs.coefficients - rhs.coefficients)

}
*/