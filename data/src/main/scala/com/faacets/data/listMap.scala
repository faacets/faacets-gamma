package com.faacets
package data

import scala.collection.immutable.ListMap

import cats.data.{NonEmptyList => NEL}

import consolidate._

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

  implicit def listMapMerge[V:Merge]: Merge[ListMap[String, V]] = new ListMapMerge[V]

}
