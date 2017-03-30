package com.faacets
package operation
package lifting
/*
import spire.algebra.Eq

import net.alasc.math.{Grp, Perm, Domain}

import data._

import core._
import core.perm.{Relabeling, PartyRelabeling}

import scala.util.parsing.combinator._

final class GroupingParsable extends Parsable[Grouping] {
  object Parser extends ParserTrait with GroupingParser {
    def phrase = grouping
  }
  def toText(g: Grouping) = g.toString
}

trait GroupingParser extends RegexParsers {
  def number: Parser[Int] = """\d+""".r ^^ { k => k.toInt }
  def inputNoGrouping: Parser[InputGrouping] = number ^^ { InputGrouping.noLifting(_) }
  def inputWithGrouping: Parser[InputGrouping] = "{" ~ rep1(number) ~ "}" ^^ {
    case "{" ~ outputs ~ "}" =>
      val blocks: Iterable[Set[Int]] = outputs.zipWithIndex.groupBy(_._1).map { case (k, v) => v.map(_._2).toSet }
      InputGrouping(Domain.Partition(blocks.toSeq: _*))
  }
  def inputGrouping: Parser[InputGrouping] = inputNoGrouping | inputWithGrouping
  def partyGrouping: Parser[PartyGrouping] = "(" ~ rep1(inputGrouping) ~ ")" ^^ {
    case "(" ~ inputGroupings ~ ")" => PartyGrouping(inputGroupings)
  }
  def grouping: Parser[Grouping] = "[" ~ rep1(partyGrouping) ~ "]" ^^ {
    case "[" ~ partyGroupings ~ "]" => Grouping(partyGroupings)
  }
}
*/