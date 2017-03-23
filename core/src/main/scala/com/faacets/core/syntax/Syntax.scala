package com.faacets.core.syntax

trait LiteralsSyntax {

    implicit def literals(sc: StringContext): Literals = new Literals(sc)

}

trait AllSyntax extends
  LiteralsSyntax
