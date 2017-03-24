package com.faacets.data

import cats.data.{Validated, ValidatedNel}

import fastparse.noApi._

/** Defines an object whose string serialization can be parsed using a fastparse parser. */
trait Parsable[T] extends Textable[T] {

  /** Parser for the represented type `T`, consuming the end of input. */
  def phrase: Parser[T]

  def toText(t: T): String

  def fromText(string: String): ValidatedNel[String, T] =
    phrase.parse(string) match {
      case Parsed.Success(t, _) => Validated.valid(t)
      case f => Validated.invalidNel(f.toString)
    }

}

object Parsable {

  def apply[T](implicit ev: Parsable[T]): Parsable[T] = ev

}
