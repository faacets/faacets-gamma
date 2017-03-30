package com.faacets
package operation
package lifting
/*
import net.alasc.math.{Grp, Perm, Domain}

import data._

import core._
import core.perm.{Relabeling, PartyRelabeling}
import util._

import scala.util.parsing.combinator._

final class LiftingParsable extends Parsable[Lifting] {
  object Parser extends ParserTrait with GroupingParser {
    def lifting = grouping ~ "->" ~ grouping ^^ {
      case source ~ "->" ~ target => Lifting(source, target)
    }

    def phrase = lifting
  }
  def toText(l: Lifting) = l.source.toText + " -> " + l.target.toText 
}
*/