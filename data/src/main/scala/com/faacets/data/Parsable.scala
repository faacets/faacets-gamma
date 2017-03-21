package com.faacets.data

import cats.data.Validated

import fastparse.noApi._

trait Parsable[T] extends Textable[T] {

  /** Parser for the represented type `T`, consuming the end of input. */
  def phrase: Parser[T]

  def toText(t: T): String

  def fromText(string: String): Validated[String, T] =
    phrase.parse(string) match {
      case Parsed.Success(t, _) => Validated.Valid(t)
      case f => Validated.Invalid(f.toString)
    }

}
