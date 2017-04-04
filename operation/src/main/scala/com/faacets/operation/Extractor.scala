package com.faacets
package operation

import cats.kernel.Comparison
import com.faacets.core.{Expr, LexicographicOrder, Relabeling}
import io.circe.{Encoder, Json}
import net.alasc.domains.Partition
import spire.algebra._
import spire.algebra.partial.{Groupoid, PartialAction}
import spire.math.{Rational, SafeLong}
import spire.util.Opt
import spire.syntax.partialAction._
import spire.syntax.groupoid._
import syntax.extractor._
import io.circe.syntax._
import com.faacets.data.instances.textable._
import com.faacets.operation.product.ExprProductExtractor
import com.faacets.operation.reordering.LexicographicPartyOrder
import scalin.immutable.Vec
import spire.syntax.order._
import spire.syntax.cfor._

trait Extractor[V] {

  def canExtract(v: V): Boolean

}

trait ProductExtractor[V] {

  def cwa: CanonicalWithAffineExtractor[V]

  def partialExtract(v: V): Opt[PolyProduct[V]]

  def forceExtract(v: V): PolyProduct[V] = partialExtract(v) getOrElse(PolyProduct.ofSingle(cwa(v)))

}

object ProductExtractor {

  implicit val expr: ProductExtractor[Expr] = new ExprProductExtractor

  def apply[V](implicit ev: ProductExtractor[V]): ProductExtractor[V] = ev

  def bind2sub(N: Seq[SafeLong], i: SafeLong): Seq[SafeLong] =
    N.scanLeft((SafeLong(0), i)) { case ((rem, quot), n) => (quot % n, quot / n) }.map(_._1).tail

  def bsub2ind(N: Seq[SafeLong], I: Seq[SafeLong]): SafeLong =
    (N zip I).foldLeft((SafeLong(0), SafeLong(1))) { case ((tot, mul), (n, i)) => (tot + mul * i, mul * n) }._1

  def ind2sub(N: Seq[Int], i: Int): Seq[Int] =
    N.scanLeft((0, i)) { case ((rem, quot), n) => (quot % n, quot / n) }.map(_._1).tail

  def sub2ind(N: Seq[Int], I: Seq[Int]): Int =
    (N zip I).foldLeft((0, 1)) { case ((tot, mul), (n, i)) => (tot + mul * i, mul * n) }._1

  def integerToVector(k: Int, n: Int): Vector[Boolean] =
    Vector.tabulate(n)(i => (k & (1 << i)) != 0)

  def nonTrivialBipartitions(n: Int): IndexedSeq[Vector[Boolean]] = {
    val bipartitions = (1 until (1 << n) - 1).map(integerToVector(_, n))
    val (singleA, restA) = bipartitions.partition(_.count(_ == true) == 1)
    val (singleB, rest) = restA.partition(_.count(_ == false) == 1)
    singleA ++ (singleB.map(_.map(!_))) ++ rest
  }

  // TODO: remove this helper?
  def allBipartitions(n: Int): IndexedSeq[Partition] = new IndexedSeq[Partition] {
    val bitset = scala.collection.immutable.BitSet(0 until n: _*)
    def length = ((1 << n) - 2)/2 // 2^n possibilities - 2 (we remove the two cases with an empty block)
    def apply(index: Int) = {
      val bits = index + 1 // the integer 0 is a bit vector representing a partition with an empty block
      val (block0, block1) = bitset.partition(b => (bits & (1 << b)) == 0)
      Partition(block0, block1)
    }
  }

}


trait OperationExtractor[V, O] extends Extractor[V] { self =>

  implicit def partialAction: PartialAction[V, O]

  implicit def groupoid: Groupoid[O]

  def identity(v: V): O

  def canExtract(v: V): Boolean = partialExtract(v).nonEmpty

  /** If the given element `e` is not reduced, finds an operation `o` such that the reduced
    * value is given by `u = e <|+| o` and returns Opt(o) or Opt.empty[O]
    */
  def extractOperation(v: V): Opt[O]

  def partialExtract(v: V): Opt[ExtractedOperation[V, O]] = extractOperation(v) match {
    case Opt(o) => Opt(ExtractedOperation(v, o))
    case _ => Opt.empty[ExtractedOperation[V, O]]
  }

  def forceExtract(v: V): ExtractedOperation[V, O] = partialExtract(v).getOrElse(ExtractedOperation(v, identity(v)))

}

object OperationExtractor {

  def apply[V, O](implicit ev: OperationExtractor[V, O]): OperationExtractor[V, O] = ev

}

trait GroupOperationExtractor[V, O] extends OperationExtractor[V, O] { self =>

  implicit def group: Group[O]

  def identity(v: V): O = group.empty

  def groupoid = new Groupoid[O] {

    def inverse(o: O): O = group.inverse(o)

    def partialOp(x: O, y: O): Opt[O] = Opt(group.combine(x, y))

  }

}

trait GroupActionOperationExtractor[V, O] extends GroupOperationExtractor[V, O] { self =>

  implicit def action: Action[V, O]

  def partialAction = new PartialAction[V, O] {

    def partialActr(v: V, o: O): Opt[V] = Opt(action.actr(v, o))

    def partialActl(o: O, v: V): Opt[V] = Opt(action.actl(o, v))

  }

}
