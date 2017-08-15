package com.faacets.core

import spire.math.Rational
import scalin.immutable.Mat


case class Table(m: Mat[Rational]) {

  override def toString = scalin.Printer.mat(m, Int.MaxValue, Int.MaxValue)

}

object Table {

  implicit def convToString(table: Table): String = table.toString

}
