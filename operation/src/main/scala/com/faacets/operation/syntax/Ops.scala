package com.faacets
package operation
package syntax

import spire.algebra.partial.PartialAction
import spire.util._

import spire.syntax.partialAction._
// import net.alasc.math.enum.Representative

final class ExtractorValueOps[A](val lhs: A) extends AnyVal {

  def partialExtract[O](implicit ev: OperationExtractor[A, O]): Opt[O] = ev.partialExtract(lhs)

  def forceExtract[O](implicit ev: OperationExtractor[A, O]): O = ev.forceExtract(lhs)

  def extracted[O](implicit ev: OperationExtractor[A, O], ev1: PartialAction[A, O]): (O, A) = ev.extracted(lhs)

  def canExtract[O](implicit ev: OperationExtractor[A, O]): Boolean = ev.canExtract(lhs)

}

/*
final class SymmetricFormsOps[T](val lhs: T)(implicit ev: SymmetricForms[T]) {
  def symmetricRepresentative: Representative[T, Relabeling] = ev.symmetricRepresentative(lhs)
  def cyclicRepresentative: Representative[T, Relabeling] = ev.cyclicRepresentative(lhs)
}
*/
