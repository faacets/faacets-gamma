package com.faacets.data

import cats.data.Validated
import io.circe._

trait Textable[T] {

  def toText(t: T): String

  def fromText(string: String): Validated[String, T]

}

object Textable {

  def apply[T](implicit T: Textable[T]): Textable[T] = T

  implicit def textableEncoder[T:Textable]: Encoder[T] = Encoder.encodeString.contramap[T](Textable[T].toText(_))

  implicit def textableDecoder[T:Textable]: Decoder[T] = ForcedStringDecoder.flatMap[T](
    str => Textable[T].fromText(str) match {
      case Validated.Valid(t) => Decoder.const(t)
      case Validated.Invalid(err) => Decoder.failedWithMessage(err)
    }
  )

}
