package com.faacets
package data

import io.circe._

import spire.math.Rational

import consolidate.Merge

import scalin.immutable.Vec

import instances.rational._

trait VecRationalInstances {

  implicit val vecRationalEncoder: Encoder[Vec[Rational]] =
    Encoder[IndexedSeq[Rational]].contramap[Vec[Rational]](_.toIndexedSeq.toVector)

  implicit val vecRationalDecoder: AccumulatingDecoder[Vec[Rational]] =
    AccumulatingDecoder[IndexedSeq[Rational]].map(seq => scalin.immutable.DenseVec(seq: _*))

  implicit val vecRationalMerge: Merge[Vec[Rational]] = Merge.fromEquals[Vec[Rational]] // TODO: move to cats.Eq

}
