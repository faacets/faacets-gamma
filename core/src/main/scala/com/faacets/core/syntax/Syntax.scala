package com.faacets.core.syntax

trait LiteralsSyntax {

    implicit def coreLiterals(sc: StringContext): Literals = new Literals(sc)

}

trait AllSyntax extends
  LiteralsSyntax
