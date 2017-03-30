package com.faacets.core

import scalin.immutable.Mat
import spire.math.Rational


case class Table(m: Mat[Rational]) {

  override def toString = scalin.Printer.mat(m, Int.MaxValue, Int.MaxValue)

}

object Table {

  implicit def convToString(table: Table): String = table.toString

}
