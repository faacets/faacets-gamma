package com.faacets
package operation
package affine

import spire.algebra.Action
import spire.syntax.group._

import com.faacets.core._


final class VecAffineAction[V <: GenExpr[V]] extends Action[V, Affine] {

  def actl(a: Affine, v: V) = actr(v, a.inverse)

  def actr(v: V, a: Affine): V = (a.multiplier *: v) + a.shift

}
