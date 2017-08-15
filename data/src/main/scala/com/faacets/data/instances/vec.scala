package com.faacets.data.instances

import spire.math.Rational

import com.faacets.consolidate.Merge
import io.circe._
import scalin.immutable.{DenseVec, Vec}
import com.faacets.data.instances.rational._
import spire.algebra.Group
import spire.algebra.partial.PartialAction
import spire.util.Opt

import net.alasc.algebra._

final class VecPermutation[A, G:PermutationAction:Group] extends PartialAction[Vec[A], G] {

  import spire.syntax.action._
  import spire.syntax.group._

  import net.alasc.syntax.permutationAction._

  override def actlIsDefined(g: G, v: Vec[A]) = g.largestMovedPoint.getOrElseFast(-1) < v.length

  override def actrIsDefined(v: Vec[A], g: G) = g.largestMovedPoint.getOrElseFast(-1) < v.length

  def partialActl(g: G, v: Vec[A]): Opt[Vec[A]] =
    if (g.largestMovedPoint.getOrElseFast(-1) >= v.length) Opt.empty[Vec[A]] else
      Opt(DenseVec.tabulate(v.length)( k => v(k <|+| g) ))

  def partialActr(v: Vec[A], g: G): Opt[Vec[A]] = partialActl(g.inverse, v)

}

trait VecInstances {

  implicit def vecPermutation[A, G:PermutationAction:Group]: PartialAction[Vec[A], G] = new VecPermutation[A, G]

  implicit val vecRationalEncoder: Encoder[Vec[Rational]] =
    Encoder[IndexedSeq[Rational]].contramap[Vec[Rational]](_.toIndexedSeq.toVector)

  implicit val vecRationalDecoder: Decoder[Vec[Rational]] =
    Decoder[IndexedSeq[Rational]].map(seq => scalin.immutable.DenseVec(seq: _*))

  implicit val vecRationalMerge: Merge[Vec[Rational]] = Merge.fromEquals[Vec[Rational]] // TODO: move to cats.Eq

}
