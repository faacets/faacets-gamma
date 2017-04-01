package com.faacets
package operation
package syntax

import spire.util._

// import net.alasc.math.enum.Representative

final class ExtractorValueOps[A](val lhs: A) extends AnyVal {

  def partialExtract[O](implicit ev: OperationExtractor[A, O]): Opt[O] = ev.partialExtract(lhs)

  def forceExtract[O](implicit ev: OperationExtractor[A, O]): O = ev.forceExtract(lhs)

  def canExtract[O](implicit ev: OperationExtractor[A, O]): Boolean = ev.canExtract(lhs)

}

/*
final class SymmetricFormsOps[T](val lhs: T)(implicit ev: SymmetricForms[T]) {
  def symmetricRepresentative: Representative[T, Relabeling] = ev.symmetricRepresentative(lhs)
  def cyclicRepresentative: Representative[T, Relabeling] = ev.cyclicRepresentative(lhs)
}
*/
