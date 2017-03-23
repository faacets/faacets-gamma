package com.faacets
package core
package perm

import spire.algebra.Monoid
import spire.syntax.monoid._

/*
final class RelabelingPrettyString extends Pretty[Relabeling, String] {
  def flow = StringFlow
  def pretty(r: Relabeling): String = r.toString
}

final class RelabelingPrettyTex extends Pretty[Relabeling, Tex] {
  def flow = Tex.Flow
  def pretty(r: Relabeling): Tex = r.components.flatMap(c => Seq(Monoid[Tex].id, c.pretty[Tex])).tail.reduce(_ |+| _)
}

final class RelabelingComponentPrettyTex extends Pretty[Relabeling.Component, Tex] {
  def flow = Tex.Flow
  def pretty(c: Relabeling.Component): Tex = c match {
    case Relabeling.OutputComponent(p, x, a) => Tex.raw(Party.prefixes(p) + x.toString + "^{" + (a + 1).toString + "}")
    case Relabeling.InputComponent(p, x) => Tex.raw(Party.prefixes(p) + "^{" + (x + 1).toString + "}")
    case Relabeling.PartyComponent(p) => Tex.raw((p + 1).toString)
  }
}
 */
