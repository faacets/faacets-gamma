package com.faacets.operation

import spire.algebra.partial.{Groupoid, PartialAction}
import spire.syntax.groupoid._

case class ExtractedOperation[V, O](val original: V, val operation: O) {
  def extracted(implicit pa: PartialAction[V, O]): V = pa.partialActr(original, operation).get
  /** Returns a pair (v, op) such that v is nondegenerate and v <|+| op is the original element. */
  def extractedPair(implicit pa: PartialAction[V, O], g: Groupoid[O]): (V, O) = (extracted, operation.inverse)
}

trait Extracted[E] {

  def original: E

}
