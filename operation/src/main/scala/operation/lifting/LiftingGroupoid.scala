package com.faacets
package operation
package lifting
/*
import spire.algebra.NullboxGroupoid
import spire.syntax.eq._
import spire.util.Nullbox

final class LiftingGroupoid extends NullboxGroupoid[Lifting] {
  def inverse(l: Lifting) = Lifting(l.target, l.source)
  override def opIsDefined(x: Lifting, y: Lifting) =
    x.target === y.source
  def partialOp(x: Lifting, y: Lifting) =
    if (opIsDefined(x, y)) Nullbox(op(x, y)) else Nullbox.empty[Lifting]
  override def op(x: Lifting, y: Lifting) = Lifting(x.source, y.target)
}
*/