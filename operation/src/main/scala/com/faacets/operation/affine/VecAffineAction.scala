package com.faacets
package operation
package affine
/*
import scala.util.Random

import spire.algebra.{Group, Eq, Action, VectorSpace}
import spire.math.Rational
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.vectorSpace._
import spire.syntax.ring._

import net.alasc.util._

import data._
import core._

import affine._

final class VecAffineAction[V <: Vec[V]](implicit vb: VecBuilder[V]) extends Action[V, Affine] {
  def actl(a: Affine, v: V) = actr(v, a.inverse)
  def actr(v: V, a: Affine): V = {
    implicit def VVectorSpace: VectorSpace[V, Rational] = v.scenario.VecVectorSpace[V]
      (a.multiplier *: v) :+ a.shift
  }
}
*/