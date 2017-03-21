package com.faacets.data
package syntax

import cats.data.Validated

/*
import net.alasc.algebra.Check

import consolidate._
 */
final class TextableStringOps(val lhs: String) extends AnyVal {

  /** Parses the string as `T`, and throws an exception if it fails. */
  def parseUnsafe[T:Textable]: T = parse match {
    case Validated.Invalid(err) => throw new IllegalArgumentException(err)
    case Validated.Valid(t) => t
  }

  def parse[T:Textable]: Validated[String, T] = Textable[T].fromText(lhs)

}

final class TextableOps[T](val lhs: T) extends AnyVal {

  def toText(implicit T:Textable[T]): String = T.toText(lhs)

}
/*
final class MergedCheckOps[A](val merge: Merged[A]) extends AnyVal {

  def withCheck(check: Check[A]): Merged[A] = merge.flatMap { a =>
    val errors = check.check(a)
    if (errors.isEmpty) MSame(a) else MFail(a, MLog(errors.groupBy(_._1).mapValues(_.map(_._2).mkString(" and "))))
  }

}
*/
