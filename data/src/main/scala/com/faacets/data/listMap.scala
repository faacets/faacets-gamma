package com.faacets
package data

import scala.collection.immutable.ListMap

import cats.data.{NonEmptyList => NEL}

import consolidate._

/*
import argonaut._, Argonaut._

/**
  * Deserializer for ListMap[String,V] types.
  */
final class ListMapStringDecodeJson[V:DecodeJson] extends DecodeJson[ListMap[String, V]] {

  def decode(a: HCursor) = a.fields match {
    case None => DecodeResult.fail("[V]ListMap[String, V]", a.history)
    case Some(s) =>

      def spin(x: List[JsonField], m: DecodeResult[ListMap[String, V]]): DecodeResult[ListMap[String, V]] =
        x match {
          case Nil => m
          case hd :: tl =>
            spin(tl, m.flatMap(mm => a.get(hd)(DecodeJson.of[V]).map(v => mm + (hd -> v))))
        }

      spin(s, DecodeResult.ok(ListMap.empty[String, V]))

  }

 }*/

final class ListMapMerge[V](implicit V: Merge[V]) extends Merge[ListMap[String, V]] {

  import cats.syntax.all._

  import Result.{same, updated}

  def merge(base: ListMap[String, V], other: ListMap[String, V]): Result[ListMap[String, V]] = {
    (same(base) /: other) {
      case (merged, (otherKey, otherValue)) =>
        val res = base get otherKey match {
          case None => updated(otherKey -> otherValue, NEL.of(Path.empty -> s"new value for key $otherKey = $otherValue"))
          case Some(baseValue) => V.merge(baseValue, otherValue).map(otherKey -> _)
        }
        (merged |@| res.in(otherKey)).map { case (accMap, kv) => accMap  + kv }
    }
  }

}


trait ListMapInstances {
/*
  implicit def listMapStringEncodeJson[V:EncodeJson]: EncodeJson[ListMap[String, V]] =
    EncodeJson[ListMap[String, V]](lm => Json.obj(lm.toSeq.map { case (k, v) => (k, EncodeJson.of[V].encode(v)) }: _*))

  implicit def listMapStringDecodeJson[V:DecodeJson]: DecodeJson[ListMap[String, V]] =
    new ListMapStringDecodeJson[V]*/

  implicit def listMapMerge[V:Merge]: Merge[ListMap[String, V]] = new ListMapMerge[V]

}
