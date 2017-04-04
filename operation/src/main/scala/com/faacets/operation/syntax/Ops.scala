package com.faacets
package operation
package syntax

import spire.algebra.partial.PartialAction
import spire.util._

import spire.syntax.partialAction._
// import net.alasc.math.enum.Representative

final class ExtractorValueOps[E](val lhs: E) extends AnyVal {

  def partialExtract[O](implicit ev: OperationExtractor[E, O]): Opt[Extracted.Operation[E, O]] = ev.partialExtract(lhs)

  def forceExtract[O](implicit ev: OperationExtractor[E, O]): Extracted.Operation[E, O] = ev.forceExtract(lhs)

  def canExtract[O](implicit ev: OperationExtractor[E, O]): Boolean = ev.canExtract(lhs)

}

/*
final class SymmetricFormsOps[T](val lhs: T)(implicit ev: SymmetricForms[T]) {
  def symmetricRepresentative: Representative[T, Relabeling] = ev.symmetricRepresentative(lhs)
  def cyclicRepresentative: Representative[T, Relabeling] = ev.cyclicRepresentative(lhs)
}
*/
