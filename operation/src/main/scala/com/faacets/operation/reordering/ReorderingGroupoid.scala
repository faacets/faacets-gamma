package com.faacets
package operation
package reordering
/*
import spire.algebra.partial.Groupoid
import spire.syntax.eq._
import spire.util.Opt

final class ReorderingGroupoid extends Groupoid[Reordering] {

  def inverse(r: Reordering) = Reordering(r.target, r.source)

  override def opIsDefined(x: Reordering, y: Reordering): Boolean = x.target === y.source

  def partialOp(x: Reordering, y: Reordering): Opt[Reordering] =
    if (opIsDefined(x, y)) Opt(Reordering(x.source, y.target)) else Opt.empty[Reordering]

}
*/