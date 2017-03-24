package com.faacets.data.syntax

import io.circe.{AccumulatingDecoder, Json}

final class JsonOps(val lhs: Json) extends AnyVal {

  def asAcc[A](implicit d: AccumulatingDecoder[A]): AccumulatingDecoder.Result[A] = d(lhs.hcursor)

}

trait JsonSyntax {

  implicit def dataJson(lhs: Json): JsonOps = new JsonOps(lhs)

}
