package com.faacets.core
package text

import cats.data.{Validated, ValidatedNel}
import scalin.immutable.VecEngine
import scalin.immutable.Vec
import spire.algebra.Ring
import scalin.syntax.all._
import spire.math.Rational
import cats.syntax.traverse._
import cats.instances.vector._
import spire.syntax.cfor._

sealed case class TermType(name: String)

object TermType {
  def constant: TermType = TermType("Constant")
  def full: TermType = TermType("Full probabilities")
  def collinsGisin: TermType = TermType("Collins-Gisin")
  def correlators: TermType = TermType("Correlators")
}

abstract class Term(val termType: TermType) { self =>

  def validate(scenario: Scenario): ValidatedNel[String, DExpr]

  def string: String

}

object Term {

  def oneAt[A:Ring:VecEngine](length: Int, ind: Int): Vec[A] =
    VecEngine[A].fromMutable(length, Ring[A].zero)( res => res(ind) := Ring[A].one )

  def parseExpression(expression: String): ValidatedNel[String, Vector[(Rational, String, Term)]] = {
    import fastparse.noApi._
    import com.faacets.data.Parsers.White._
    (CoeffString.expr ~ End).parse(expression) match {
      case Parsed.Success(csSeq, _) =>
        val allCoeffTerms = csSeq.toVector.map {
          case CoeffString(coeff, None) => Validated.valid((coeff, "", ConstantTerm))
          case CoeffString(coeff, Some(termString)) =>
            (CoeffString.term ~ End).parse(termString) match {
              case Parsed.Success(term, _) => Validated.valid((coeff, termString, term))
              case f => Validated.invalidNel(f.toString)
            }
        }
        allCoeffTerms.sequenceU
      case f => Validated.invalidNel(f.toString)
    }
  }

  def printHeadCoeff(sb: StringBuilder, coeff: Rational, term: String): Unit = {
    require(!coeff.isZero)
    if (coeff.signum < 0) {
      sb ++= "-"
      printHeadCoeff(sb, -coeff, term)
    } else {
      if (coeff.isOne) {
        if (term.isEmpty)
          sb ++= "1"
        else
          sb ++= term
      } else {
        sb ++= coeff.toString
        if (term.nonEmpty) {
          sb ++= " "
          sb ++= term
        }
      }
    }
  }

  def printTailCoeff(sb: StringBuilder, coeff: Rational, term: String): Unit = {
    require(coeff.signum != 0)
    if (coeff.signum < 0) {
      sb ++= " - "
      printHeadCoeff(sb, -coeff, term)
    } else {
      sb ++= " + "
      printHeadCoeff(sb, coeff, term)
    }
  }

  def printExpression(seq: Seq[(Rational, Term)]) = {
    val sb = StringBuilder.newBuilder
    if (seq.length > 0) printHeadCoeff(sb, seq(0)._1, seq(0)._2.string)
    cforRange(1 until seq.length) { i =>
      printTailCoeff(sb, seq(i)._1, seq(i)._2.string)
    }
    sb.toString
  }

}

case object ConstantTerm extends Term(TermType.constant) {

  def validate(scenario: Scenario): ValidatedNel[String, DExpr] = Validated.valid(Expr.constant(scenario).toDExpr)

  def string = ""

}

/** */
case class FullTerm(outputs: Seq[Int], inputs: Seq[Int]) extends Term(TermType.full) {
  import scalin.immutable.dense._

  def nOutputs: Int = outputs.size

  def nInputs: Int = inputs.size

  def validate(scenario: Scenario): ValidatedNel[String, DExpr] =
  if (outputs.size != inputs.size)
    Validated.invalidNel(s"Term has $nOutputs outputs but $nInputs inputs")
  else if (outputs.size != scenario.nParties)
    Validated.invalidNel(s"Term is written for $nOutputs parties but scenario has ${scenario.nParties} parties")
  else {
    val oiTuplesValidated = (scenario.parties zip (outputs zip inputs)).toVector.map {
      case (party, (a, x)) if x >= party.nInputs => Validated.invalidNel(s"Input $x for a party with ${party.nInputs} inputs (note: indices are 0-based!)")
      case (party, (a, x)) if a >= party.nOutputs(x) => Validated.invalidNel(s"Output $a for an input with ${party.nOutputs(x)} outputs (note: indices are 0-based!)")
      case (party, (a, x)) => Validated.valid( (a, x) )
    }
    oiTuplesValidated.sequenceU.map { tupleSeq =>
      val (aSeq, iSeq) = tupleSeq.unzip
      DExpr(scenario, Term.oneAt[Rational](scenario.shapeP.size, scenario.sub2indP(aSeq.toArray, iSeq.toArray)))
    }
  }

  def string = "P(" + outputs.mkString(",") + "|" + inputs.mkString(",") + ")"

}

case class CGTerm(parties: Seq[Int], outputs: Seq[Int], inputs: Seq[Int]) extends Term(TermType.collinsGisin) {
  import scalin.immutable.dense._

  def nOutputs: Int = outputs.size

  def nInputs: Int = inputs.size

  def nParties: Int = parties.size

  def validate(scenario: Scenario): ValidatedNel[String, DExpr] =
    if (outputs.size != inputs.size)
      Validated.invalidNel(s"Term has $nOutputs outputs but $nInputs inputs")
    else if (outputs.size != parties.size)
      Validated.invalidNel(s"Number of parties is inconsistent $nParties != $nOutputs")
    else {
      val partyPosition = parties.zipWithIndex.toMap
      if (partyPosition.size < parties.size)
        Validated.invalidNel("Party cannot appear more than once")
      else if (parties.max >= scenario.nParties)
        Validated.invalidNel("Party in term not present in the scenario")
      else {
        val kxTuplesValidated = (0 until scenario.nParties).toVector.map { p =>
          partyPosition.get(p) match {
            case None => Validated.valid( (-1, -1) )
            case Some(i) =>
              val k = outputs(i)
              val x = inputs(i)
              if (x >= scenario.parties(p).nInputs)
                Validated.invalidNel(s"Term using input $x for a party with ${scenario.parties(p).nInputs} inputs (note: indices are 0-based")
              else if (k == scenario.parties(p).inputs(x) - 1)
                Validated.invalidNel(s"Collins-Gisin term using the last output $k for an input with ${scenario.parties(p).inputs(x)} outputs; this output is not present in the Collins-Gisin representation")
              else if (k >= scenario.parties(p).inputs(x))
                Validated.invalidNel(s"Term using the output $k for an input with ${scenario.parties(p).inputs(x)} outputs (note: indices are 0-based)")
              else Validated.valid( (k, x) )
          }
        }
        kxTuplesValidated.sequenceU.map { tupleSeq =>
          val (kSeq, xSeq) = tupleSeq.unzip
          val expr = Expr.collinsGisin(scenario, Term.oneAt[Rational](scenario.shapeNG.size, scenario.sub2indNG(kSeq.toArray, xSeq.toArray)))
          expr.toDExpr
        }
      }

    }

  def string = "P" + parties.map(p => ('A' + p).toChar.toString).mkString + "(" + outputs.mkString(",") + "|" + inputs.mkString(",") + ")"

}

object CGTerm {

  def fromKX(kk: Seq[Int], xx: Seq[Int]): Term = {
    val nParties = kk.length
    require(kk.length == xx.length)
    val parties = (0 until nParties).filter(p => kk(p) >= 0 && xx(p) >=0 )
    if (parties.isEmpty) ConstantTerm else {
      val outputs = parties.map(kk(_))
      val inputs = parties.map(xx(_))
      CGTerm(parties, outputs, inputs)
    }
  }

}

case class CorrelatorsTerm(elements: Seq[(Int, Int)]) extends Term(TermType.correlators) {
  import scalin.immutable.dense._

  def validate(scenario: Scenario): ValidatedNel[String, DExpr] =
  if (scenario.maxNumOutputs > 2)
    Validated.invalidNel(s"Invalid use of correlators in a scenario with an input having ${scenario.maxNumOutputs}>2 outputs")
  else {
    val partyToInputIndex = elements.toMap
    if (partyToInputIndex.size < elements.size) Validated.invalidNel(s"Correlator contains more than one operator per party")
    else {
      val kxTuplesValidated = (0 until scenario.nParties).toVector.map { p =>
        partyToInputIndex.get(p) match {
          case None => Validated.valid( (-1, -1 ) ) // marginal element
          case Some(x) if x >= scenario.parties(p).nInputs =>
            Validated.invalidNel(s"Correlator for input $x for party with ${scenario.parties(p).nInputs} inputs (note: indices are 0-based!)")
          case Some(x) => Validated.valid( (0, x) )
          }
      }
      kxTuplesValidated.sequenceU.map { tupleSeq =>
        val (kSeq, xSeq) = tupleSeq.unzip
        val expr = Expr.correlators(scenario, Term.oneAt[Rational](scenario.shapeNC.size, scenario.sub2indNC(kSeq.toArray, xSeq.toArray)))
        expr.toDExpr
      }

    }
  }

  def string = "<" + (elements.map { case (p, x) => ('A' + p).toChar.toString + x.toString }.mkString) + ">"

}

object CorrelatorsTerm {

  def fromKX(kk: Seq[Int], xx: Seq[Int]): Term = {
    val nParties = kk.length
    require(kk.length == xx.length)
    val parties = (0 until nParties).filter(p => kk(p) >= 0 && xx(p) >=0 )
    if (parties.isEmpty) ConstantTerm else {
      val outputs = parties.map(kk(_))
      assert(outputs.forall(_ == 0))
      val inputs = parties.map(xx(_))
      CorrelatorsTerm(parties zip inputs)
    }
  }

}
