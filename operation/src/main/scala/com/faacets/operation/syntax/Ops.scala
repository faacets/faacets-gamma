package com.faacets
package operation
package syntax

import com.faacets.core.Relabeling
import spire.util._

final class ExtractorValueOps[E](val lhs: E) extends AnyVal {

  def partialExtract[O](implicit ev: OperationExtractor[E, O]): Opt[ExtractedOperation[E, O]] = ev.partialExtract(lhs)

  def forceExtract[O](implicit ev: OperationExtractor[E, O]): ExtractedOperation[E, O] = ev.forceExtract(lhs)

  def canExtract[O](implicit ev: OperationExtractor[E, O]): Boolean = ev.canExtract(lhs)

}

final class SymmetricFormsOps[T](val lhs: T)(implicit ev: SymmetricForms[T]) {

  def symmetricRepresentative: ExtractedOperation[T, Relabeling] = ev.symmetricRepresentative(lhs)

  def cyclicRepresentative: ExtractedOperation[T, Relabeling] = ev.cyclicRepresentative(lhs)

}
