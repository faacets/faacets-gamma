package com.faacets.operation

import spire.math.Rational
import spire.algebra.Action
import spire.syntax.action._

/** Represents a tensor product as a tree.
  * Not all polynomials can be expressed in that way, only those that can be written as a Horner scheme:
  * ((x*y + scalar) * (z*t + scalar) + scalar) ...
  */
sealed trait ProductTree[A] {
  def size: Int
  /** Applies tensoring and shifts to compute the represented expression. */
  def merged(implicit A: Action[A, Affine], T: Tensor[A]): A
  def toPolyProduct: PolyProduct[A]
  def map[B](f: A => B): ProductTree[B]
}

object ProductTree {

  /** Node in a product tree
    *
    * @param parts Maps indices to components; must have parts.keySet.flatten = {0, ..., n-1}
    */
  final case class Node[A](parts: Map[Set[Int], ProductTree[A]], affine: Affine) extends ProductTree[A] {
    require(parts.forall { case (k,v) => k.size == v.size })
    def size = parts.keySet.flatten.size

    def merged(implicit A: Action[A, Affine], T: Tensor[A]): A =
      T(parts.mapValues(_.merged)) <|+| affine

    def toPolyProduct: PolyProduct[A] =
      Tensor[PolyProduct[A]].apply(parts.mapValues(_.toPolyProduct)) <|+| affine

    def map[B](f: A => B): Node[B] = Node(parts.mapValues(_.map(f)), affine)
  }

  final case class Leaf[A](a: A, n: Int, affine: Affine) extends ProductTree[A] {
    def size = n
    def merged(implicit A: Action[A, Affine], T: Tensor[A]): A = a <|+| affine
    def toPolyProduct: PolyProduct[A] = {
      val part = Set(0 until n: _*)
      val coeffs = Map(Set(part) -> affine.multiplier, Set.empty[Set[Int]] -> affine.shift).filterNot(_._2.isZero)
      PolyProduct(Map(part -> a), coeffs)
    }
    def map[B](f: A => B): Leaf[B] = Leaf(f(a), n, affine)
  }

}
