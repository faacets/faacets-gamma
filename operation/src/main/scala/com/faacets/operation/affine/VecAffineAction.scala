package com.faacets
package operation
package affine

import spire.algebra.Action
import spire.syntax.group._

import com.faacets.core._


final class VecAffineAction[V[S] <: GenExpr[V, S], S <: Scenario with Singleton] extends Action[V[S], Affine] {

  def actl(a: Affine, v: V[S]): V[S] = actr(v, a.inverse)

  def actr(v: V[S], a: Affine): V[S] = (a.multiplier *: v) + a.shift

}
