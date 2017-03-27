package com.faacets.data.instances

import scala.collection.immutable.ListMap

import com.faacets.consolidate._
import com.faacets.consolidate.syntax.merge._
import com.faacets.consolidate.instances.all._
import io.circe._

final class JsonObjectMerge extends Merge[JsonObject] {

  def merge(current: JsonObject, other: JsonObject): Result[JsonObject] = {

    implicit val mergeJson: Merge[Json] = Merge.fromEquals[Json]

    (ListMap(current.toList: _*) merge ListMap(other.toList: _*)).map(JsonObject.fromIterable)

  }

}

trait JsonObjectInstances {

  implicit val jsonObjectMerge: Merge[JsonObject] = new JsonObjectMerge

}
