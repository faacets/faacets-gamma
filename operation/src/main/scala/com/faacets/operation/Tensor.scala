package com.faacets.operation

import com.faacets.core.{Behavior, DExpr, Expr, Scenario}
import com.faacets.operation.product.PVecTensor
import spire.algebra.Order
import spire.math.Sorting
import spire.std.int._
import spire.syntax.cfor._

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

trait Tensor[V] {
  /** Generalized tensor product that allows reordering of the result. */

  def apply(components: Map[Set[Int], V]): V
}

object Tensor {
  def apply[V](implicit V: Tensor[V]): Tensor[V] = V
  implicit val expr: Tensor[Expr] = new PVecTensor[Expr]
  implicit val dExpr: Tensor[DExpr] = new PVecTensor[DExpr]
  implicit val behavior: Tensor[Behavior] = new PVecTensor[Behavior]

}

case class SetPartition[A](parts: Set[Set[A]]) {

  def baseSet: Set[A] = parts.flatten

  def size: Int = parts.foldLeft(0)( (sz, set) => sz + set.size )
  /** Returns the elements of this set partition as an ordered sequence of ordered sequences.
    * Similar to "standard_form" in SAGE.
    */

  def toSeqOfSeqs(implicit order: Order[A]): Seq[Seq[A]] = {
    implicit val ordering: Ordering[A] = order.toOrdering
    parts.map(_.toSeq.sorted).toSeq.sortBy(_.apply(0))
  }

  /** Returns the restriction of this SetPartition to a subset of it. */
  def restriction(subset: Set[A]): SetPartition[A] =
    SetPartition(parts.map(set => set.intersect(subset)).filterNot(_.isEmpty))

  /** Returns the set partition obtained by applying an injective function to the elements of the set.
    *
    * @param f  An injective map A => B (i.e. that preserves distinctness)
    * @return the transformed set partition
    */
  def map[B](f: A => B): SetPartition[B] = SetPartition(parts.map(_.map(f)))

  def flatMap[B](f: A => Set[B]): SetPartition[B] = SetPartition(parts.map(_.flatMap(f)))

}

object SetPartition {

  /** Given a set, returns all nontrivial bipartitions of the set. */
  def nonTrivialBipartitions[A](set: Set[A]): Set[SetPartition[A]] = {
    val ordered = set.toSeq
    val n = ordered.size
    // each block of the given partition can be either in the first or second block of the bipartition
    // we iterate over bitstrings of length n
    // 0 -> trivial, and by symmetry only considers half the bitstrings
    (1 until (1 << (n - 1))).toSet.map { (k: Int) =>
      val inFirst = BitSet.fromBitMask(Array(k.toLong))
      val allSet = (1 << n) - 1
      val inSecond = BitSet.fromBitMask(Array((allSet - k).toLong))
      val firstPart = inFirst.map(ordered)
      val secondPart = inSecond.map(ordered)
      SetPartition[A](Set(firstPart, secondPart))
    }
  }

}
