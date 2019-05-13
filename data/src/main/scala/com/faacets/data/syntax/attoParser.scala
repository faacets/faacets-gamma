package com.faacets.data.syntax

import atto.Parser

final class AttoParserOps[A](val lhs: Parser[A]) extends AnyVal {

  import atto.Atto.{err, ok}

  /** Ensures, if the parse has been successful so far, that a property of the result satisfies a predicate.
    * If not, returns a failed parse with an explanatory message. */
  def check[B](property: A => B)(predicate: B => Boolean, message: (A, B) => String): Parser[A] =
    lhs.flatMap { a =>
      val b = property(a)
      if (!predicate(b)) err(message(a, b)) else ok(a)
    }

}

trait AttoParserSyntax {

  implicit def attoParser[A](lhs: Parser[A]): AttoParserOps[A] = new AttoParserOps(lhs)

}
