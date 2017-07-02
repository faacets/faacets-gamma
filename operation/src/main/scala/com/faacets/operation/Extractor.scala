package com.faacets
package operation

import com.faacets.core.Expr
import spire.algebra._
import spire.algebra.partial.{Groupoid, PartialAction}
import spire.util.Opt
import com.faacets.operation.product.ExprProductExtractor

trait Extractor[V] {

  def canExtract(v: V): Boolean

}

trait ProductExtractor[V] {

  implicit def cwa: CanonicalWithAffineExtractor[V]

  def partialExtract(v: V): Opt[PolyProduct[CanonicalDec[V]]]

  def forceExtract(v: V): PolyProduct[CanonicalDec[V]] = partialExtract(v) getOrElse(PolyProduct.ofSingle(v))

}


object ProductExtractor {
  implicit val expr: ProductExtractor[Expr] = new ExprProductExtractor

  def apply[V](implicit ev: ProductExtractor[V]): ProductExtractor[V] = ev

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
