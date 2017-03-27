package com.faacets.data.instances

import spire.math.Rational

import com.faacets.consolidate.Merge
import io.circe._
import scalin.immutable.Vec
import com.faacets.data.instances.rational._

trait VecInstances {

  implicit val vecRationalEncoder: Encoder[Vec[Rational]] =
    Encoder[IndexedSeq[Rational]].contramap[Vec[Rational]](_.toIndexedSeq.toVector)

  implicit val vecRationalDecoder: AccumulatingDecoder[Vec[Rational]] =
    AccumulatingDecoder[IndexedSeq[Rational]].map(seq => scalin.immutable.DenseVec(seq: _*))

  implicit val vecRationalMerge: Merge[Vec[Rational]] = Merge.fromEquals[Vec[Rational]] // TODO: move to cats.Eq

}
