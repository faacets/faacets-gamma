package com.faacets.operation

import cats.kernel.Comparison
import com.faacets.core.{LexicographicOrder, Relabeling, Scenario}
import io.circe.{Encoder, Json}
import net.alasc.domains.Partition
import spire.algebra._
import spire.algebra.partial.{Groupoid, PartialAction}
import spire.math.Rational
import spire.syntax.partialAction._
import spire.syntax.groupoid._
import syntax.extractor._
import io.circe.syntax._
import com.faacets.data.instances.textable._
import com.faacets.operation.reordering.LexicographicPartyOrder
import scalin.immutable.Vec
import spire.syntax.order._
import spire.syntax.cfor._

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


case class CanonicalDecWithAffine[V](affine: Affine,
                                     lifting: Lifting,
                                     reordering: Reordering,
                                     relabeling: Relabeling,
                                     canonical: V) {
  def original(implicit A: PartialAction[V, Affine], L: PartialAction[V, Lifting], O: PartialAction[V, Reordering], R: PartialAction[V, Relabeling]): V = {
    val step1 = (canonical <|+|? relabeling).get
    val step2 = (step1 <|+|? reordering).get
    val step3 = (step2 <|+|? lifting).get
    val step4 = (step3 <|+|? affine).get
    step4
  }
  def originalScenario: Scenario = lifting.target.scenario
  def canonicalScenario: Scenario = reordering.source
  def withoutAffine: (Affine, CanonicalDec[V]) = (affine, CanonicalDec(lifting, reordering, relabeling, canonical))
}


object CanonicalDecWithAffine {

  import CanonicalDec.someIfNotId

  implicit def encoder[V:Encoder]: Encoder[CanonicalDecWithAffine[V]] = new Encoder[CanonicalDecWithAffine[V]] {
    def apply(ld: CanonicalDecWithAffine[V]): Json = {
      val fields = Seq(
        someIfNotId(ld.affine).map(a => "affine" -> a.asJson),
        someIfNotId(ld.lifting).map(l => "lifting" -> l.asJson),
        someIfNotId(ld.reordering).map(o => "reordering" -> o.asJson),
        someIfNotId(ld.relabeling).map(r => "relabeling" -> r.asJson)
      ).flatten :+ ("canonical" -> ld.canonical.asJson)
      Json.obj(fields: _*)
    }
  }

}

case class ExtractedOperation[V, O](val original: V, val operation: O) {
  def extracted(implicit pa: PartialAction[V, O]): V = pa.partialActr(original, operation).get
  /** Returns a pair (v, op) such that v is nondegenerate and v <|+| op is the original element. */
  def extractedPair(implicit pa: PartialAction[V, O], g: Groupoid[O]): (V, O) = (extracted, operation.inverse)
}

case class PartitionPolynomial(partition: Partition, coeffs: Map[Set[Int], Rational]) {

  override def toString = {
    import com.faacets.core.text.Term.printSeq
    val ct = coeffs.toSeq.map {
      case (in, coeff) => (coeff, in.toVector.sorted.map(b => partition.blocks(b).toVector.sorted.map(i => ('A' + i).toChar.toString).mkString).mkString("x"))
    }.sortBy(_._2)
    printSeq(ct)
  }

}

case class PolyProduct[V](pp: PartitionPolynomial, extracted: Vector[CanonicalDec[V]]) {
  def allCanonicals: Seq[V] = extracted.map(_.canonical)
  def original(additive: Groupoid[V])(implicit A: PartialAction[V, Affine], L: PartialAction[V, Lifting], O: PartialAction[V, Reordering], R: PartialAction[V, Relabeling], P: Tensor[V]): V = {
    import pp.{partition, coeffs}
    val n = partition.nBlocks
    require(extracted.length == n)
    val originals = extracted.map(_.original)
    val zeros = originals.map(v => additive.leftId(v))
    val ones = zeros.map(v => (v <|+|? Affine(1, 1)).get)
    val zero = P(partition, zeros)
    coeffs.foldLeft(zero) {
      case (acc, (in, coeff)) =>
        val exprs = Vector(0 until n: _*).map { b => if (in.contains(b)) originals(b) else ones(b) }
        val expr = (P(partition, exprs) <|+|? Affine(coeff, 0)).get
        additive.partialOp(acc, expr).get
    }
  }
}

object PolyProduct {

  def merge[V](partition: Partition, left: PolyProduct[V], right: PolyProduct[V]): PolyProduct[V] = {
    require(partition.nBlocks == left.pp.partition.nBlocks + right.pp.partition.nBlocks)
    require(partition.size == left.pp.partition.size + right.pp.partition.size)
    ???
  }

  def ofSingle[V](cwa: CanonicalDecWithAffine[V]): PolyProduct[V] = {
    val partition = Partition(Set(0 until cwa.originalScenario.nParties: _*))
    val (Affine(m, s), c) = cwa.withoutAffine
    val poly = Map(Set.empty[Int] -> s, Set(0) -> m)
    PolyProduct(PartitionPolynomial(partition, poly), Vector(c))
  }
}

trait CanonicalWithAffineExtractor[V] {

  def apply(v: V): CanonicalDecWithAffine[V]

}

object CanonicalWithAffineExtractor {

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
          case Some(Comparison.EqualTo) | Some(Comparison.LessThan) => CanonicalDecWithAffine(a, l, o, rPlus, vPlus)
          case Some(Comparison.GreaterThan) => CanonicalDecWithAffine(a.copy(multiplier = -a.multiplier), l, o, rMinus, vMinus)
          case _ => sys.error("Cannot happen, both are in the same scenario")
        }
      }
    }

}

trait Extracted[E] {

  def original: E

}
