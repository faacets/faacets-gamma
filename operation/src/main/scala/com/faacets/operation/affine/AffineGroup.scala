package com.faacets
package operation
package affine

import spire.algebra.{Eq, Group}
import spire.math.Rational
import spire.syntax.eq._

final class AffineEq extends Eq[Affine] {

  def eqv(u: Affine, v: Affine) = (u.multiplier === v.multiplier) && (u.shift === v.shift)

}

/** Composition of affine transformations. */
final class AffineGroup extends Group[Affine] {

  def inverse(u: Affine) = Affine(u.multiplier.reciprocal, -u.shift/u.multiplier)

  /** Composition of affine transformations.
    * 
    * If u = Affine(a, b), with x <|+| u = a x + b,
    * and v = Affine(c, d), with x <|+| v = c x + d,
    * then u * v = Affine(a * c, b * c + d) with
    * x <|+| u * v = (x <|+| u) <|+| v = c * (a x + b) + d.
    */
  def combine(u: Affine, v: Affine) = (u, v) match {
    case (Affine(a, b), Affine(c, d)) => Affine(a * c, b * c + d)
  }

  def empty = Affine(Rational.one, Rational.zero)

}
