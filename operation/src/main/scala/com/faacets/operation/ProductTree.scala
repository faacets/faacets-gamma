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
  def original(implicit A: Action[A, Affine], T: Tensor[A]): A
  def toPolyProduct: PolyProduct[A]
  def map[B](f: A => B): ProductTree[B]
  /** Extracts a possible affine transform from the elements A.
    *
    * @param f Function such that f(a) = (f, b) and "a = b <|+| f" (in spirit)
    */
  def mapAffine[B](f: A => (Affine, B)): ProductTree[B]
  def elements: Iterable[A]
}

object ProductTree {

  /** Node in a product tree
    *
    * @param parts Maps indices to components; must have parts.keySet.flatten = {0, ..., n-1}
    */
  final case class Node[A](parts: Map[Set[Int], ProductTree[A]], affine: Affine) extends ProductTree[A] {
    require(parts.forall { case (k,v) => k.size == v.size })
    def size = parts.keySet.flatten.size

    def original(implicit A: Action[A, Affine], T: Tensor[A]): A =
      T(parts.mapValues(_.original)) <|+| affine

    def toPolyProduct: PolyProduct[A] =
      Tensor[PolyProduct[A]].apply(parts.mapValues(_.toPolyProduct)) <|+| affine

    def map[B](f: A => B): Node[B] = Node(parts.mapValues(_.map(f)), affine)

    def mapAffine[B](f: (A) => (Affine, B)): ProductTree[B] =
      Node(parts.mapValues(_.mapAffine(f)), affine)

    def elements = parts.values.flatMap(_.elements)
  }

  final case class Leaf[A](a: A, n: Int, affine: Affine) extends ProductTree[A] {
    def size = n
    def original(implicit A: Action[A, Affine], T: Tensor[A]): A = a <|+| affine
    def toPolyProduct: PolyProduct[A] = {
      val part = Set(0 until n: _*)
      val coeffs = Map(Set(part) -> affine.multiplier, Set.empty[Set[Int]] -> affine.shift).filterNot(_._2.isZero)
      PolyProduct(Map(part -> a), coeffs)
    }
    def map[B](f: A => B): Leaf[B] = Leaf(f(a), n, affine)
    def mapAffine[B](f: (A) => (Affine, B)): ProductTree[B] = {
      val (affine1, b) = f(a)
      // m*(m1 * b + s1) + s = m*m1*b + s1*m + s
      val newAffine = Affine(affine.multiplier*affine1.multiplier, affine1.shift*affine.multiplier + affine.shift)
      Leaf(b, n, newAffine)
    }
    def elements = Iterable(a)
  }

}
