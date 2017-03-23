package com.faacets
package data

import consolidate._

import io.circe._

import com.faacets.consolidate._

import Merge.syntax._

import scala.collection.immutable.ListMap

import instances.listMap._

final class JsonObjectMerge extends Merge[JsonObject] {

  def merge(current: JsonObject, other: JsonObject): Result[JsonObject] = {

    implicit val mergeJson: Merge[Json] = Merge.fromEquals[Json]

    (ListMap(current.toList: _*) merge ListMap(other.toList: _*)).map(JsonObject.fromIterable)

  }

}

trait JsonObjectInstances {

  implicit val jsonObjectMerge: Merge[JsonObject] = new JsonObjectMerge

}
