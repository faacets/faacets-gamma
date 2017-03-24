package com.faacets.data

import cats.data.{NonEmptyList => Nel, Validated, ValidatedNel}
import io.circe._

import syntax.validatedNel._

/** Defines an object that serializes using a string representation. */
trait Textable[T] {

  def toText(t: T): String

  def fromText(string: String): ValidatedNel[String, T]

}

object Textable {

  def apply[T](implicit T: Textable[T]): Textable[T] = T

  implicit def textableEncoder[T:Textable]: Encoder[T] = Encoder.encodeString.contramap[T](Textable[T].toText(_))

  implicit def textableDecoder[T:Textable]: AccumulatingDecoder[T] = AccumulatingDecoder.instance[T] { cursor =>
    ForcedStringDecoder.accumulating(cursor).andThen { str =>
      Textable[T].fromText(str).toAccumulatingDecoderResult
    }
  }

  class ParseException(val errors: Nel[String]) extends IllegalArgumentException(errors.toList.mkString(","))

}
