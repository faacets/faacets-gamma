package com.faacets

import scala.annotation.tailrec

import spire.math.SafeLong

/** Package dealing with Bell scenarios and associated objects (inequalities, correlations...). */
package object core {

  /*

  /* Cartesian product of traversable. */
  def combine[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] =
    xs.foldLeft(Seq(Seq.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }

  def combineList[A](xs: Traversable[Traversable[A]]): Seq[List[A]] =
    xs.foldLeft(Seq(List.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }

  def bind2sub(N: Seq[SafeLong], i: SafeLong): Seq[SafeLong] =
    N.scanLeft((SafeLong(0), i)) { case ((rem, quot), n) => (quot % n, quot / n) }.map(_._1).tail

  def bsub2ind(N: Seq[SafeLong], I: Seq[SafeLong]): SafeLong =
    (N zip I).foldLeft((SafeLong(0), SafeLong(1))) { case ((tot, mul), (n, i)) => (tot + mul * i, mul * n) }._1

  def ind2sub(N: Seq[Int], i: Int): Seq[Int] =
    N.scanLeft((0, i)) { case ((rem, quot), n) => (quot % n, quot / n) }.map(_._1).tail

  def sub2ind(N: Seq[Int], I: Seq[Int]): Int =
    (N zip I).foldLeft((0, 1)) { case ((tot, mul), (n, i)) => (tot + mul * i, mul * n) }._1

*/
}
