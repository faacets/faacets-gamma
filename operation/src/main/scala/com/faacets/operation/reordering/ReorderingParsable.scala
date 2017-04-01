package com.faacets
package operation
package reordering
/*
import spire.algebra.{Eq, Groupoid, PartialAction}
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.eq._

import data._

import core._

import qalg.immutable.QVector

final class ReorderingParsable extends Parsable[Reordering] {
  object Parser extends ParserTrait with ScenarioParser {
    def phrase = (scenario <~ "->") ~ scenario ^^ {
      case source ~ target => Reordering(source, target)
    }
  }
  def toText(r: Reordering) = r.source.toText + " -> " + r.target.toText
}
*/