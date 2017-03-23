package com.faacets.data

import cats.data.{NonEmptyList => Nel, Validated, ValidatedNel}
import io.circe._

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

  class TextableParseException(val errors: Nel[String]) extends IllegalArgumentException(errors.toList.mkString(","))

  final class TextableStringOps(val lhs: String) extends AnyVal {

    /** Parses the string as `T`, and throws an exception if it fails. */
    def parseUnsafe[T:Textable]: T = parse match {
      case Validated.Invalid(errors) => throw new TextableParseException(errors)
      case Validated.Valid(t) => t
    }

    def parse[T:Textable]: ValidatedNel[String, T] = Textable[T].fromText(lhs)

  }

  final class TextableOps[T](val lhs: T) extends AnyVal {

    def toText(implicit T:Textable[T]): String = T.toText(lhs)

  }

  object syntax {

    implicit def textableStringOps(lhs: String): TextableStringOps = new TextableStringOps(lhs)

    implicit def textableOps[T](lhs: T): TextableOps[T] = new TextableOps[T](lhs)

  }

}
