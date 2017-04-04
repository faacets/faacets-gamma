package com.faacets.core

import spire.math.Rational
import spire.algebra.VectorSpace
import spire.algebra.partial.Groupoid
import spire.math.Rational
import spire.util.Opt
import spire.syntax.eq._
import scalin.immutable.dense._

trait GenExpr[V <: GenExpr[V]] extends PVec[V] { lhs: V =>

  def builder: GenExprBuilder[V]

  /** Computes the inner product between this expression and the given behavior. */
  def inner(rhs: Behavior): Rational = lhs.coefficients.dot(rhs.coefficients)

  def :*(r: Rational): V = r *: lhs

  def *:(r: Rational): V = {
    if (r.isZero) builder.zero(scenario)
      else builder.updatedWithSymmetryGroup(lhs, scenario, coefficients * r, grp => Some(grp))
  }

  def unary_- : V = builder.updatedWithSymmetryGroup(lhs, scenario, -coefficients, grp => Some(grp))

  def +(rhs: V): V = {
    require(lhs.scenario === rhs.scenario)
    builder.apply(scenario, lhs.coefficients + rhs.coefficients)
  }

  def -(rhs: V): V = {
    require(lhs.scenario === rhs.scenario)
    builder.apply(scenario, lhs.coefficients - rhs.coefficients)
  }

  def +(rhs: Rational): V = lhs + rhs *: builder.one(lhs.scenario)

  def -(rhs: Rational): V = lhs - rhs *: builder.one(lhs.scenario)

}


trait GenExprBuilder[V <: GenExpr[V]] extends PVecBuilder[V] { self =>

  def zero(scenario: Scenario): V

  def one(scenario: Scenario): V

  protected def groupoid: Groupoid[V] = new Groupoid[V] {
    def inverse(a: V): V = -a
    def partialOp(x: V, y: V): Opt[V] =
      if (x.scenario === y.scenario) Opt(x + y) else Opt.empty[V]
  }

  implicit val additiveGrouopid: AdditiveGroupoid[V] = AdditiveGroupoid(groupoid)

  def vectorSpace(scenario: Scenario): VectorSpace[V, Rational] = new VectorSpace[V, Rational] {

    def scalar = spire.math.Rational.RationalAlgebra

    def timesl(r: Rational, v: V): V = {
      require(v.scenario === scenario)
      r *: v
    }

    def negate(x: V): V = {
      require(x.scenario === scenario)
      -x
    }

    def zero: V = self.zero(scenario)

    def plus(x: V, y: V): V = {
      require(x.scenario === scenario)
      require(y.scenario === scenario)
      x + y
    }

  }

}
