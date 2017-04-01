package com.faacets
package operation
/*
import spire.algebra.{PartialAction, Group, Groupoid, Semigroup}
import spire.math.Rational
import spire.syntax.partialAction._
import spire.syntax.groupoid._
import spire.util._

import net.alasc.math.Domain

/** Type class for extractable operations. */ 
trait Extractor[A] {
  def canExtract(a: A): Boolean
}

case class ProductShape(partition: Domain#Partition, shift: Rational)

trait ProductExtractor[A] extends Extractor[A] {
  implicit def semigroup: Semigroup[A]

  def canExtract(a: A): Boolean = partialExtract(a).nonEmpty
  def partialExtract(a: A): Nullbox[ProductShape]
}

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


trait OperationExtractor[A, O] extends Extractor[A] {
  implicit def action: PartialAction[A, O]
  implicit def groupoid: Groupoid[O]

  def identity(a: A): O

  def canExtract(a: A): Boolean = partialExtract(a).nonEmpty

  /** If the given element `a` is not reduced, finds an operation `o` such that the reduced
    * value is given by `u = a <|+| o` and returns Nullbox(o).
    */
  def partialExtract(a: A): Nullbox[O]

  def forceExtract(a: A): O = partialExtract(a).getOrElse(identity(a))
}

trait GroupOperationExtractor[A, O] extends OperationExtractor[A, O] {
  implicit def group: Group[O]
  def groupoid = group
  def identity(a: A): O = group.id
}

/*
case class Extractors[T](extractors: Seq[Extractor[T]]) {
  def partialExtractTree(leaf: Leaf[T])(implicit lb: LeafBuilder[T]): Option[Branch[T]] = {
    extractors.foreach { ext =>
      ext.partialExtractTree(leaf) match {
        case someNode@Some(node) => return someNode
        case None =>
      }
    }
    None
  }
  def repeatPartialExtractTree(tree: Tree[T])(implicit lb: LeafBuilder[T]): Option[Tree[T]] = tree match {
    case leaf: Leaf[T] => partialExtractTree(leaf).map( branch => repeatPartialExtractTree(branch) match {
      case None => branch
      case Some(extracted) => extracted
    })
    case oNode: OperationNode[T, _] => repeatPartialExtractTree(oNode.child).map(newChild => oNode.childUpdated(newChild))
    case pNode: ProductNode[T] =>
      val newChildrenOption = pNode.children.map(repeatPartialExtractTree(_))
      if (newChildrenOption.forall(_.isEmpty))
        None
      else {
        val newChildren = (newChildrenOption zip pNode.children).map {
          case (Some(child), _) => child
          case (None, oldChild) => oldChild
        }
        Some(pNode.childrenUpdated(newChildren))
      }
  }
}
 */
*/