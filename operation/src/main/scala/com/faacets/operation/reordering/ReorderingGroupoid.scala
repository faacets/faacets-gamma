package com.faacets
package operation
package reordering
/*
import spire.algebra.{Eq, NullboxGroupoid, PartialAction}
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.eq._
import spire.util.Nullbox

import data._

import core._
import core.perm._

import qalg.immutable.QVector

final class ReorderingGroupoid extends NullboxGroupoid[Reordering] {
  def inverse(r: Reordering) = Reordering(r.target, r.source)
  override def opIsDefined(x: Reordering, y: Reordering): Boolean = x.target === y.source
  override def op(x: Reordering, y: Reordering): Reordering = {
    assert(x.target === y.source)
    Reordering(x.source, y.target)
  }
  def partialOp(x: Reordering, y: Reordering): Nullbox[Reordering] =
    if (opIsDefined(x, y)) Nullbox(op(x, y)) else Nullbox.empty[Reordering]
}
*/