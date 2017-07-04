package com.faacets.operation

import cats.kernel.Comparison
import com.faacets.core.{LexicographicOrder, Relabeling, Scenario}
import spire.algebra.partial.PartialAction
import io.circe.{Encoder, Json}
import io.circe.syntax._
import spire.syntax.partialAction._
import spire.syntax.groupoid._
import syntax.extractor._
import com.faacets.data.instances.textable._

case class CanonicalDecWithAffine[V](affine: Affine,
                                     lifting: Option[Lifting],
                                     reordering: Option[Reordering],
                                     relabeling: Relabeling,
                                     canonical: V) {
  def original(implicit A: PartialAction[V, Affine], L: PartialAction[V, Lifting], O: PartialAction[V, Reordering], R: PartialAction[V, Relabeling]): V = {
    val step1 = (canonical <|+|? relabeling).get
    val step2 = reordering.fold(step1)(ro => (step1 <|+|? ro).get)
    val step3 = lifting.fold(step2)(li => (step2 <|+|? li).get)
    val step4 = (step3 <|+|? affine).get
    step4
  }
  def splitAffine: (Affine, CanonicalDec[V]) = (affine, CanonicalDec(lifting, reordering, relabeling, canonical))
  def map[B](f: V => B): CanonicalDec[B] = CanonicalDec(lifting, reordering, relabeling, f(canonical))
}

object CanonicalDecWithAffine {

  implicit def encoder[V:Encoder]: Encoder[CanonicalDecWithAffine[V]] = Encoder.instance[CanonicalDecWithAffine[V]] { cd =>
    Json.obj(
      "affine" -> cd.affine.asJson,
      "lifting" -> cd.lifting.asJson,
      "reordering" -> cd.reordering.asJson,
      "relabeling" -> cd.relabeling.asJson,
      "canonical" -> cd.canonical.asJson
    )
  }

}

trait CanonicalWithAffineExtractor[V] {

  def apply(v: V): CanonicalDecWithAffine[V]

}

object CanonicalWithAffineExtractor {

  def apply[V](implicit ev: CanonicalWithAffineExtractor[V]): CanonicalWithAffineExtractor[V] = ev

  implicit def forV[V: LexicographicOrder](implicit
                                           A: PartialAction[V, Affine], AE: OperationExtractor[V, Affine],
                                           L: PartialAction[V, Lifting], LE: OperationExtractor[V, Lifting],
                                           O: PartialAction[V, Reordering], OE: OperationExtractor[V, Reordering],
                                           R: PartialAction[V, Relabeling], RE: OperationExtractor[V, Relabeling]
                                          ): CanonicalWithAffineExtractor[V] =
    new CanonicalWithAffineExtractor[V] {
      def apply(original: V) = {
        val (res1, l) = original.forceExtract[Lifting].extractedPair
        val (res2, o) = res1.forceExtract[Reordering].extractedPair
        val (res3, a) = res2.forceExtract[Affine].extractedPair
        val (vPlus, rPlus) = res3.forceExtract[Relabeling].extractedPair
        val res3neg = (res3 <|+|? Affine(-1, 0)).get
        val (vMinus, rMinus) = res3neg.forceExtract[Relabeling].extractedPair
        LexicographicOrder[V].partialComparison(vPlus, vMinus) match {
          case Some(Comparison.EqualTo) | Some(Comparison.LessThan) =>
            CanonicalDecWithAffine(a,
              if (l.isId) None else Some(l), if (o.isId) None else Some(o), rPlus, vPlus)
          case Some(Comparison.GreaterThan) =>
            CanonicalDecWithAffine(a.copy(multiplier = -a.multiplier),
              if (l.isId) None else Some(l), if (o.isId) None else Some(o), rMinus, vMinus)
          case _ => sys.error("Cannot happen, both are in the same scenario")
        }
      }
    }

}
