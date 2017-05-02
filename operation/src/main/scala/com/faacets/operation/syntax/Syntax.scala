package com.faacets
package operation
package syntax

trait ExtractorSyntax {
  implicit def extractorValueOps[A](a: A) = new ExtractorValueOps[A](a)
}

trait SymmetricFormsSyntax {
  implicit def symmetricFormsOps[T: SymmetricForms](t: T): SymmetricFormsOps[T] =
    new SymmetricFormsOps(t)
}

trait AllSyntax
  extends ExtractorSyntax
    with SymmetricFormsSyntax
