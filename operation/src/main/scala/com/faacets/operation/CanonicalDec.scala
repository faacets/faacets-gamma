package com.faacets.operation

import spire.algebra.Eq
import spire.algebra.partial.Groupoid
import com.faacets.core.{Relabeling, Scenario}
import io.circe.{Encoder, Json}
import spire.algebra.partial.PartialAction
import io.circe.syntax._
import spire.syntax.partialAction._
import com.faacets.data.instances.textable._

/** Canonical decomposition, however without applying an affine transform. */
case class CanonicalDec[V](lifting: Lifting, reordering: Reordering, relabeling: Relabeling, canonical: V) {
  def original(implicit L: PartialAction[V, Lifting], O: PartialAction[V, Reordering], R: PartialAction[V, Relabeling]): V = {
    val step1 = (canonical <|+|? relabeling).get
    val step2 = (step1 <|+|? reordering).get
    val step3 = (step2 <|+|? lifting).get
    step3
  }
  def originalScenario: Scenario = lifting.target.scenario
  def canonicalScenario: Scenario = reordering.source
  def withAffine(affine: Affine): CanonicalDecWithAffine[V] =
    CanonicalDecWithAffine(affine, lifting, reordering, relabeling, canonical)
}

object CanonicalDec {

  def someIfNotId[O:Eq:Groupoid](op: O): Option[O] = if (Groupoid[O].isId(op)) None else Some(op)

  implicit def encoder[V:Encoder]: Encoder[CanonicalDec[V]] = new Encoder[CanonicalDec[V]] {
    def apply(ld: CanonicalDec[V]): Json = {
      val fields = Seq(
        someIfNotId(ld.lifting).map(l => "lifting" -> l.asJson),
        someIfNotId(ld.reordering).map(o => "reordering" -> o.asJson),
        someIfNotId(ld.relabeling).map(r => "relabeling" -> r.asJson)
      ).flatten :+ ("canonical" -> ld.canonical.asJson)
      Json.obj(fields: _*)
    }
  }

}
