package com.faacets
package data

import io.circe._

import spire.math.Rational

// import consolidate.Merge
// import rational._

import scalin.Vec
import scalin.immutable.{Vec => IVec}
// import scalin.immutable.dense._
// import scalin.syntax.all._

// import argonaut._, Argonaut._

trait VecRationalInstances {

  import rational._

  implicit val vecRationalEncoder: Encoder[Vec[Rational]] =
    Encoder[IndexedSeq[Rational]].contramap[Vec[Rational]](_.toIndexedSeq.toVector)

  implicit val vecRationalDecoder: Decoder[IVec[Rational]] =
    Decoder[IndexedSeq[Rational]].map(seq => scalin.immutable.DenseVec(seq: _*))
/*
  implicit val vecRationalMerge: Merge[Vec[Rational]] = Merge.fromEquals[Vec[Rational]]

 */
}
