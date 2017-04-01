package com.faacets
package operation
package relabeling
/*
import spire.algebra.{PartialAction, Action}
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.partialOrder._
import spire.util._

import polyta._

import net.alasc.algebra.InversePair
import net.alasc.math.Grp
import net.alasc.util._

import core._
import perm.{Relabeling, ShapeLattice}
import relabeling._

class VecRelabelingExtractor[V <: Vec[V]](
  implicit builder: VecBuilder[V],
  val action: PartialAction[V, Relabeling],
  rep: VecRepresentatives[V]) extends GroupOperationExtractor[V, Relabeling] {
  def group = Relabeling.Algebra
  def partialExtract(vec: V): Nullbox[Relabeling] = {
    val p = rep.representatives(vec).head.element
    // TODO: do minimal right coset with fixed based sym. subgroup to have always
    // the same answer for the permutation
    if (vec.symmetryGroup.contains(p)) Nullbox.empty[Relabeling] else Nullbox(p) // TODO: check if it is not the inverse that is required
  }
}
*/