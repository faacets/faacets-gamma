package com.faacets
package operation
package syntax
/*
import scala.language.experimental.macros

import machinist.{DefaultOps => Ops}

import spire.util._

import net.alasc.math.enum.Representative

import core.perm.Relabeling

final class ExtractorValueOps[A](val lhs: A) extends AnyVal {
  def partialExtract[O](implicit ev: OperationExtractor[A, O]): Nullbox[O] = ev.partialExtract(lhs)
  def forceExtract[O](implicit ev: OperationExtractor[A, O]): O = ev.forceExtract(lhs)
  def canExtract[O](implicit ev: OperationExtractor[A, O]): Boolean = ev.canExtract(lhs)
}

/*
final class ExtractorLeafOps[T: LeafBuilder](val lhs: Leaf[A]) {
  def partialExtractTree[O](implicit ev: OperationExtractor[T, O]): Option[OperationNode[T, O]] = ev.partialExtractTree(lhs)
}*/

final class SymmetricFormsOps[T](val lhs: T)(implicit ev: SymmetricForms[T]) {
  def symmetricRepresentative: Representative[T, Relabeling] = ev.symmetricRepresentative(lhs)
  def cyclicRepresentative: Representative[T, Relabeling] = ev.cyclicRepresentative(lhs)
}
*/