package com.faacets.data

import cats.data.{NonEmptyList => Nel, ValidatedNel}

/** Defines an object that serializes using a string representation. */
trait Textable[T] {

  def toText(t: T): String

  def fromText(string: String): ValidatedNel[String, T]

}

object Textable {

  def apply[T](implicit T: Textable[T]): Textable[T] = T

  class ParseException(val errors: Nel[String]) extends IllegalArgumentException(errors.toList.mkString(","))

}
