package com.faacets.core

import cats.kernel.Comparison
import spire.algebra.PartialOrder

/** A derivative of spire.algebra.PartialOrder that compares only the coefficients. */
trait LexicographicOrder[V] { self =>

  /** If x and y are in the same scenario, return the comparison of their coefficients,
    * under the lexicographic order. Otherwise, returns None.
    */
  def partialComparison(x: V, y: V): Option[Comparison]

  def toPartialOrder: PartialOrder[V] = new PartialOrder[V] {
    def partialCompare(x: V, y: V): Double = self.partialComparison(x, y) match {
      case Some(Comparison.GreaterThan) => 1.0
      case Some(Comparison.EqualTo) => 0.0
      case Some(Comparison.LessThan) => -1.0
      case None => Double.NaN
    }
  }

}

object LexicographicOrder {

  def apply[V](implicit ev: LexicographicOrder[V]): LexicographicOrder[V] = ev

  def fromPartialOrder[V](partialOrder: PartialOrder[V]): LexicographicOrder[V] = new LexicographicOrder[V] {

    def partialComparison(x: V, y: V): Option[Comparison] = partialOrder.partialComparison(x, y)

  }

}
