package com.faacets.data.instances

import scala.collection.immutable.ListSet

import cats.data.Validated

import io.circe._

trait ListSetInstances {

    implicit def listSetDecoder[V:Decoder] = new Decoder[ListSet[V]] {

      def apply(a: HCursor) = decodeAccumulating(a).leftMap(_.head).toEither

      override def decodeAccumulating(a: HCursor): AccumulatingDecoder.Result[ListSet[V]] =
        Decoder[Vector[V]].tryDecodeAccumulating(a) andThen { vec =>
          val listSet = ListSet(vec: _*)
          if (listSet.size == vec.size) Validated.Valid(listSet) else Validated.invalidNel(DecodingFailure("Set has duplicates", a.history))
        }

    }

    implicit def listSetEncoder[V:Encoder]: Encoder[ListSet[V]] =
      Encoder[Vector[V]].contramap(set => set.toVector)

}
