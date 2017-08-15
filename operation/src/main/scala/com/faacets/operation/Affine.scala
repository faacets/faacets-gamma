package com.faacets
package operation

import spire.algebra.{Action, Eq, Group}
import spire.math.Rational
import spire.syntax.action._
import spire.syntax.eq._
import spire.syntax.group._

import com.faacets.core._
import com.faacets.data._
import com.faacets.operation.affine._

/** Description of an affine transformation with a multiplicative factor and a shift. */
case class Affine(multiplier: Rational, shift: Rational) {

  require(multiplier =!= Rational.zero)

  def :*(r: Rational) = Affine(multiplier * r, shift * r)

  def :+(r: Rational) = Affine(multiplier, shift + r)

  def :-(r: Rational) = Affine(multiplier, shift - r)

  def isIdentity = (multiplier == 1) && (shift == 0)

  override def toString = {
    def dumpMultiplierPart: String = {
      if (multiplier == 1) ""
      else if (multiplier == -1) "-"
      else multiplier.toString + " * "
    }
    def dumpShiftPart: String = shift.signum match {
      case -1 => " - " + (-shift).toString
      case 0 => ""
      case 1 => " + " + shift.toString
    }
    dumpMultiplierPart + "x" + dumpShiftPart
  }

}

object Affine {

  implicit val textable: Textable[Affine] = Textable.fromParser[Affine](Parsers.affine, _.toString)

  implicit val group: Group[Affine] = new AffineGroup

  implicit val equ: Eq[Affine] = new AffineEq

  implicit val exprAction: Action[Expr, Affine] = new VecAffineAction[Expr]

  implicit val valueAction: Action[Value, Affine] = new Action[Value, Affine] {
    def actl(a: Affine, v: Value): Value = actr(v, a.inverse)
    def actr(v: Value, a: Affine): Value = v * a.multiplier + a.shift
  }

  implicit val boundedExprAction: Action[BoundedExpr, Affine] = new Action[BoundedExpr, Affine] {

    def actl(a: Affine, be: BoundedExpr): BoundedExpr = actr(be, a.inverse)

    def actr(be: BoundedExpr, a: Affine): BoundedExpr = {
      def boundTransform(name: String, v: Value): Option[(String, Value)] = Some((name, v <|+| a))
      val newLower = be.lower.processBounds(boundTransform)
      val newUpper = be.upper.processBounds(boundTransform)
      val newExpr = be.expr <|+| a
      BoundedExpr(newExpr, newLower, newUpper)
    }

  }

  implicit val exprExtractor: OperationExtractor[Expr, Affine] = new ExprAffineExtractor

}
