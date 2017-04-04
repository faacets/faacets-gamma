package com.faacets
package operation

import cats.kernel.Comparison
import com.faacets.core.{Expr, LexicographicOrder, Relabeling}
import io.circe.{Encoder, Json}
import net.alasc.domains.Partition
import spire.algebra._
import spire.algebra.partial.{Groupoid, PartialAction}
import spire.math.Rational
import spire.util.Opt
import spire.syntax.partialAction._
import spire.syntax.groupoid._
import syntax.extractor._
import io.circe.syntax._
import com.faacets.data.instances.textable._
import com.faacets.operation.reordering.LexicographicPartyOrder
import scalin.immutable.Vec
import spire.syntax.order._
import spire.syntax.cfor._

trait Extractor[V] {

  def canExtract(v: V): Boolean

}

trait ProductExtractor[V] {

  def cwa: CanonicalWithAffineExtractor[V]

  def partialExtract(v: V): Opt[Extracted.Product[V]]

  def forceExtract(v: V): Extracted.Product[V] = partialExtract(v) getOrElse(Extracted.Product.ofSingle(cwa(v)))

}

/*
trait Decomposition[A] {

    /*        val parties1: Seq[Party] = (expr.scenario.parties zip in1).filter(_._2).map(_._1)
     val parties2: Seq[Party] = (expr.scenario.parties zip in1).filterNot(_._2).map(_._1)
     val scenario1 = Scenario(parties1)
     val scenario2 = Scenario(parties2)
     val r = exprToDecompose.representation
     val rExpr1 = Expr(scenario1, cRepresentation, coeffs1).to(r)
     val rExpr2 = Expr(scenario2, cRepresentation, coeffs2).to(r)
     val Affine(factor1, _) = Affine.ExprExtractor.forceExtract(rExpr1)
     val Affine(factor2, _) = Affine.ExprExtractor.forceExtract(rExpr2)
     val rExpr1Norm = {
     import rExpr1.scenario.ExprVectorSpace
     rExpr1 :/ factor1
     }
     val rExpr2Norm = {
     import rExpr2.scenario.ExprVectorSpace
     rExpr2 :/ factor2
     }
     return Some((Affine(factor1 * factor2, shift), in1, rExpr1Norm, rExpr2Norm))*/
}
*/

/*
object ProductExtractor {
  def allBipartitions(n: Int): IndexedSeq[Domain#Partition] = new IndexedSeq[Domain#Partition] {
    val bitset = scala.collection.immutable.BitSet(0 until n: _*)
    def length = ((1 << n) - 2)/2 // 2^n possibilities - 2 (we remove the two cases with an empty block)
    def apply(index: Int) = {
      val bits = index + 1 // the integer 0 is a bit vector representing a partition with an empty block
      val (block0, block1) = bitset.partition(b => (bits & (1 << b)) == 0)
      Domain.Partition(block0, block1)
    }
  }
}
*/


trait OperationExtractor[V, O] extends Extractor[V] { self =>

  implicit def partialAction: PartialAction[V, O]

  implicit def groupoid: Groupoid[O]

  def identity(v: V): O

  def canExtract(v: V): Boolean = partialExtract(v).nonEmpty

  /** If the given element `e` is not reduced, finds an operation `o` such that the reduced
    * value is given by `u = e <|+| o` and returns Opt(o) or Opt.empty[O]
    */
  def extractOperation(v: V): Opt[O]

  def partialExtract(v: V): Opt[Extracted.Operation[V, O]] = extractOperation(v) match {
    case Opt(o) => Opt(Extracted.Operation(v, o))
    case _ => Opt.empty[Extracted.Operation[V, O]]
  }

  def forceExtract(v: V): Extracted.Operation[V, O] = partialExtract(v).getOrElse(Extracted.Operation(v, identity(v)))

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
