package com.faacets.data.syntax

import io.circe.{AccumulatingDecoder, Decoder, Json}

final class JsonOps(val lhs: Json) extends AnyVal {

  def asAcc[A](implicit d: Decoder[A]): AccumulatingDecoder.Result[A] = d.accumulating(lhs.hcursor)

}

trait JsonSyntax {

  implicit def dataJson(lhs: Json): JsonOps = new JsonOps(lhs)

}
