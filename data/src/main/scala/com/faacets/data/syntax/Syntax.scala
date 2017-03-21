package com.faacets
package data
package syntax

import cats.data.Validated
// import net.alasc.algebra.Check

// import consolidate._

trait TextableSyntax {

  implicit def textableStringOps(lhs: String): TextableStringOps = new TextableStringOps(lhs)

  implicit def textableOps[T](lhs: T): TextableOps[T] = new TextableOps[T](lhs)

}
/*
trait MergedCheckSyntax {

  implicit def mergedCheckOps[A](lhs: Merged[A]): MergedCheckOps[A] = new MergedCheckOps[A](lhs)

}*/

trait AllSyntax
    extends TextableSyntax
//    with MergedCheckSyntax
