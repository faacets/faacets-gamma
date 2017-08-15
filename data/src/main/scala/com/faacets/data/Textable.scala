package com.faacets.data

import cats.data.{Validated, ValidatedNel, NonEmptyList => Nel}

import fastparse.noApi._

/** Defines an object that serializes using a string representation. */
trait Textable[T] {

  def toText(t: T): String

  def fromText(string: String): ValidatedNel[String, T]

}

object Textable {

  def apply[T](implicit T: Textable[T]): Textable[T] = T

  class ParseException(val errors: Nel[String]) extends IllegalArgumentException(errors.toList.mkString(","))

  /** Creates a Textable instance from a parser and toString function.
    * The parser will parse `phrase ~ End`, for the given `phrase`.
    *
    * @param phrase  Parser for the represented `T`
    * @param toText0 toString function
    */
  def fromParser[T](phrase: Parser[T], toText0: T => String): Textable[T] = new Textable[T] {
    import Parsers.White._
    val untilEnd = Start ~ phrase ~ End
    def toText(t: T): String = toText0(t)
    def fromText(string: String): ValidatedNel[String, T] =
      untilEnd.parse(string) match {
        case Parsed.Success(t, _) => Validated.valid(t)
        case f => Validated.invalidNel(f.toString)
      }
  }

  def fromParserAndValidation[T, U](phrase: Parser[T],
                                    validation: T => ValidatedNel[String, U],
                                    toText0: U => String): Textable[U] = new Textable[U] {
    import Parsers.White._
    val untilEnd = Start ~ phrase ~ End
    def toText(u: U): String = toText0(u)
    def fromText(string: String): ValidatedNel[String, U] =
      untilEnd.parse(string) match {
        case Parsed.Success(t, _) => validation(t)
        case f => Validated.invalidNel(f.toString)
      }
  }

}
