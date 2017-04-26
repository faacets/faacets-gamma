package com.faacets.operation

import cats.kernel.Comparison
import com.faacets.core.{AdditiveGroupoid, LexicographicOrder, Relabeling, Scenario}
import io.circe.{Encoder, Json}
import net.alasc.domains.{Partition, PartitionMap}
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

case class ExtractedOperation[V, O](val original: V, val operation: O) {
  def extracted(implicit pa: PartialAction[V, O]): V = pa.partialActr(original, operation).get
  /** Returns a pair (v, op) such that v is nondegenerate and v <|+| op is the original element. */
  def extractedPair(implicit pa: PartialAction[V, O], g: Groupoid[O]): (V, O) = (extracted, operation.inverse)
}

trait Extracted[E] {

  def original: E

}
