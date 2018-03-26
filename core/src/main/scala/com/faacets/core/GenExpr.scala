package com.faacets.core

import com.faacets.core.ref.POVM
import spire.algebra.VectorSpace
import spire.math.Rational
import scalin.immutable.dense._

trait GenExpr[V[X <: Scenario with Singleton] <: GenExpr[V, X], S <: Scenario with Singleton] extends PVec[V, S] { lhs: V[S] =>

  def builder: GenExprBuilder[V]

  /** Computes the inner product between this expression and the given behavior. */
  def inner(rhs: Behavior[S]): Rational = lhs.coefficients.dot(rhs.coefficients)

  def :*(r: Rational): V[S]= r *: lhs

  def *:(r: Rational): V[S] = {
    if (r.isZero) builder.zero(scenario: S)
      else builder.updatedWithSymmetryGroup(lhs, coefficients * r, grp => Some(grp))
  }

  def unary_- : V[S] = builder.updatedWithSymmetryGroup(lhs, -coefficients, grp => Some(grp))

  def +(rhs: V[S]): V[S] = builder.apply(scenario: S, lhs.coefficients + rhs.coefficients)

  def -(rhs: V[S]): V[S] = builder.apply(scenario: S, lhs.coefficients - rhs.coefficients)

  def +(rhs: Rational): V[S] = lhs + rhs *: builder.one(lhs.scenario: S)

  def -(rhs: Rational): V[S] = lhs - rhs *: builder.one(lhs.scenario: S)
}


trait GenExprBuilder[V[X  <: Scenario with Singleton] <: GenExpr[V, X]] extends PVecBuilder[V] { self =>

  def zero(scenario: Scenario): V[scenario.type]

  def one(scenario: Scenario): V[scenario.type]

  implicit def vectorSpace[S <: Scenario with Singleton](scenario: S): VectorSpace[V[S], Rational] = new VectorSpace[V[S], Rational] {

    def scalar = spire.math.Rational.RationalAlgebra

    def timesl(r: Rational, v: V[S]): V[S] = r *: v

    def negate(x: V[S]): V[S] = -x

    def zero: V[S] = self.zero(scenario: S)

    def plus(x: V[S], y: V[S]): V[S] = x + y

  }

}
