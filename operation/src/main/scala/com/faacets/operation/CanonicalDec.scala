package com.faacets.operation
/*
import spire.algebra.partial.PartialAction
import spire.syntax.partialAction._

import io.circe._
import io.circe.syntax._

import com.faacets.core.Relabeling
import com.faacets.data.instances.textable._

/** Canonical decomposition, however without applying an affine transform. */
case class CanonicalDec[V](lifting: Option[Lifting], reordering: Option[Reordering], relabeling: Relabeling, canonical: V) {
  def original(implicit L: PartialAction[V, Lifting], O: PartialAction[V, Reordering], R: PartialAction[V, Relabeling]): V = {
    val step1 = (canonical <|+|? relabeling).get
    val step2 = reordering.fold(step1)(ro => (step1 <|+|? ro).get)
    val step3 = lifting.fold(step2)(li => (step2 <|+|? li).get)
    step3
  }
  def withAffine(affine: Affine): CanonicalDecWithAffine[V] =
    CanonicalDecWithAffine(affine, lifting, reordering, relabeling, canonical)
  def map[B](f: V => B): CanonicalDec[B] = CanonicalDec(lifting, reordering, relabeling, f(canonical))
}

object CanonicalDec {

  implicit def encoder[V:Encoder]: Encoder[CanonicalDec[V]] = Encoder.instance[CanonicalDec[V]] { cd =>
      Json.obj(
        "lifting" -> cd.lifting.asJson,
        "reordering" -> cd.reordering.asJson,
        "relabeling" -> cd.relabeling.asJson,
        "canonical" -> cd.canonical.asJson
      )
  }

  implicit def decoder[V:Decoder]: Decoder[CanonicalDec[V]] = new Decoder[CanonicalDec[V]] {
    def apply(c: HCursor): Decoder.Result[CanonicalDec[V]] = decodeAccumulating(c).leftMap(_.head).toEither

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[CanonicalDec[V]] =
      AccumulatingDecoder.resultInstance.map4(
        Decoder[Option[Lifting]].tryDecodeAccumulating(c.downField("lfiting")),
        Decoder[Option[Reordering]].tryDecodeAccumulating(c.downField("reordering")),
        Decoder[Relabeling].tryDecodeAccumulating(c.downField("relabeling")),
        Decoder[V].tryDecodeAccumulating(c.downField("canonical"))
      )( CanonicalDec(_, _, _, _) )

  }

}
*/