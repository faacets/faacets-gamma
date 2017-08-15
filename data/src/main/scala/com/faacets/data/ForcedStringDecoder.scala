package com.faacets
package data

import io.circe._

object ForcedStringDecoder extends Decoder[String] {
  import scala.util.{Left, Right}

  import Decoder.Result
  final def apply(c: HCursor): Result[String] = {
    def failure(typ: String) = Left(DecodingFailure("Textable element should not be " + typ, c.history))
    c.value.fold(
      failure("null"), // null
      x => failure("bool"), // bool
      x => Right(x.toString), // number
      x => Right(x), // string
      x => failure("array"), // array
      x => failure("object") // object
    )
  }
}
