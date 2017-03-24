package com.faacets.data
package syntax


import cats.data.{Validated, ValidatedNel}
import com.faacets.data.Textable.ParseException

final class TextableStringOps(val lhs: String) extends AnyVal {

  /** Parses the string as `T`, and throws an exception if it fails. */
  def parseUnsafe[T:Textable]: T = parse match {
    case Validated.Invalid(errors) => throw new ParseException(errors)
    case Validated.Valid(t) => t
  }

  def parse[T:Textable]: ValidatedNel[String, T] = Textable[T].fromText(lhs)

}

final class TextableOps[T](val lhs: T) extends AnyVal {

  def toText(implicit T:Textable[T]): String = T.toText(lhs)

}

trait TextableSyntax {

  implicit def textableStringOps(lhs: String): TextableStringOps = new TextableStringOps(lhs)

  implicit def textableOps[T](lhs: T): TextableOps[T] = new TextableOps[T](lhs)

}
