package com.faacets
package operation

import spire.algebra.{Action, Group}
import spire.algebra.partial.{Groupoid, PartialAction}
import spire.util.Opt

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
*/

trait OperationExtractor[E, O] { self =>

  implicit def partialAction: PartialAction[E, O]

  implicit def groupoid: Groupoid[O]

  def identity(e: E): O

  def canExtract(e: E): Boolean = partialExtract(e).nonEmpty

  /** If the given element `e` is not reduced, finds an operation `o` such that the reduced
    * value is given by `u = e <|+| o` and returns Opt(o) or Opt.empty[O]
    */
  def partialExtract(e: E): Opt[O]

  def forceExtract(e: E): O = partialExtract(e).getOrElse(identity(e))

}

trait GroupOperationExtractor[E, O] extends OperationExtractor[E, O] { self =>

  implicit def group: Group[O]

  def identity(e: E): O = group.empty

  def groupoid = new Groupoid[O] {

    def inverse(o: O): O = group.inverse(o)

    def partialOp(x: O, y: O): Opt[O] = Opt(group.combine(x, y))

  }

}

trait GroupActionOperationExtractor[E, O] extends GroupOperationExtractor[E, O] { self =>

  implicit def action: Action[E, O]

  def partialAction = new PartialAction[E, O] {

    def partialActr(p: E, g: O): Opt[E] = Opt(action.actr(p, g))

    def partialActl(g: O, p: E): Opt[E] = Opt(action.actl(g, p))

  }

}
