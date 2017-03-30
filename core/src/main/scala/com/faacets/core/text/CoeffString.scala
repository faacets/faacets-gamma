package com.faacets.core
package text

import fastparse.noApi._
import spire.math.Rational

case class CoeffString(coeff: Rational, element: Option[String]) {
  def unary_- : CoeffString = CoeffString(-coeff, element)
}

object CoeffString {

  import com.faacets.data.Parsers._
  import White._

  val probabilityString: P[String] = P("P" ~ CharIn('A'to'Z').rep ~ "(" ~ CharIn(('0'to'9')++Seq(',','|')).rep ~ ")").!

  val correlatorString: P[String] = P("<" ~ CharIn(('A'to'Z')++('0'to'9')).rep ~ ">").!

  val termString: P[String] = P(probabilityString | correlatorString)

  val nonNegativeCoeffNonEmptyString: P[CoeffString] = P( (nonNegativeRational ~ "*".?).? ~ termString ).map {
    case (None, str) => CoeffString(Rational.one, Some(str))
    case (Some(r), str) => CoeffString(r, Some(str))
  }

  val nonNegativeConstant: P[CoeffString] = P( nonNegativeRational ).map(r => CoeffString(r, None) )

  val nonNegativeCoeffString = P( nonNegativeCoeffNonEmptyString | nonNegativeConstant )

  val firstCoeffString: P[CoeffString] = P( ("+" | "-" | "").!.? ~ nonNegativeCoeffString ).map {
    case (Some("-"), term) => -term
    case (_, term) => term
  }

  val nextCoeffString: P[CoeffString] = P( ("+" | "-").! ~ nonNegativeCoeffString ).map {
    case ("-", term) => -term
    case (_, term) => term
  }

  val expr: P[Seq[CoeffString]] = P( firstCoeffString ~ nextCoeffString.rep ).map {
    case (first, next) => first +: next
  }

  val indices: P[Seq[Int]] = nonNegativeInt.rep(min=1,sep=",").opaque("<indices>")

  val condIndices: P[(Seq[Int], Seq[Int])] = P( "(" ~ indices ~ "|" ~ indices ~ ")" )

  val fullTerm: P[FullTerm] = P( "P" ~ condIndices ).map {
    case (outputs, inputs) => FullTerm(outputs, inputs)
  }

  val partySeq: P[Seq[Int]] = P( CharIn('A'to'Z').repX(min=1) ).!.map( str => str.map(_ - 'A') )

  val cgTerm: P[CGTerm] = P( "P" ~~ partySeq ~ condIndices ).map {
    case (parties, (outputs, inputs)) => CGTerm(parties, outputs, inputs)
  }

  val correlator: P[(Int, Int)] = P( CharIn('A'to'Z').! ~~ nonNegativeInt ).map {
    case (p, x) => (p.head - 'A', x)
  }

  val correlatorTerm: P[CorrelatorsTerm] = P( "<" ~ correlator.rep(min = 1) ~ ">" ).map( CorrelatorsTerm )

  val term: P[Term] = P( fullTerm | cgTerm | correlatorTerm )

}
