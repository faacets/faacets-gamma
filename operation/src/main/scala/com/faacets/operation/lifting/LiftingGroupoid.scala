package com.faacets
package operation
package lifting

import spire.algebra.partial.Groupoid
import spire.syntax.eq._
import spire.util.Opt

final class LiftingGroupoid extends Groupoid[Lifting] {

  def inverse(l: Lifting) = Lifting(l.target, l.source)

  override def opIsDefined(x: Lifting, y: Lifting) =
    x.target === y.source

  def partialOp(x: Lifting, y: Lifting) =
    if (opIsDefined(x, y)) Opt(Lifting(x.source, y.target)) else Opt.empty[Lifting]

}
