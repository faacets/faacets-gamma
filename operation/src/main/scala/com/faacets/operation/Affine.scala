package com.faacets
package operation
/*
import scala.util.Random

import spire.algebra.{Group, Eq, Action, VectorSpace}
import spire.math.Rational
import spire.syntax.eq._
import spire.syntax.vectorSpace._
import spire.syntax.ring._

import data._
import core._

import affine._

/** Description of an affine transformation with a multiplicative factor and a shift. */
case class Affine(multiplier: Rational, shift: Rational) {
  require(multiplier =!= Rational.zero)
  def :*(r: Rational) = Affine(multiplier * r, shift * r)
  def :+(r: Rational) = Affine(multiplier, shift + r)
  def :-(r: Rational) = Affine(multiplier, shift - r)
  def isIdentity = (multiplier == 1) && (shift == 0)
  override def toString = this.toText
}

object Affine {
  implicit val Parsable: Parsable[Affine] = new AffineParsable
  implicit val Group: Group[Affine] = new AffineGroup
  implicit val Eq: Eq[Affine] = new AffineEq
  implicit val ExprExtractor: OperationExtractor[Expr, Affine] = new ExprAffineExtractor
  implicit val CorrExtractor: OperationExtractor[Corr, Affine] = new CorrAffineExtractor
  implicit val ExprAction: Action[Expr, Affine] = new VecAffineAction[Expr]
  implicit val CorrAction: Action[Corr, Affine] = new VecAffineAction[Corr]
}
*/