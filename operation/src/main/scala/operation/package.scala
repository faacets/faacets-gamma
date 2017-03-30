package com.faacets
/*
import scala.language.implicitConversions

import spire.algebra._

import core._
import core.perm.Relabeling
import operation.product._
import operation.relabeling._

package object operation {
  class VecRepresentativesSyntax[V <: Vec[V]](val v: V) extends AnyVal {
    def representatives(implicit ev: VecRepresentatives[V]) = ev.representatives(v)
  }

  implicit def exprBounds(expr: Expr) = new ExprBounds(expr)

  implicit val ExprSemigroup: Semigroup[Expr] = new VecTensorSemigroup[Expr]
  implicit val CorrSemigroup: Semigroup[Corr] = new VecTensorSemigroup[Corr]
  implicit val ExprRelabelingAction: PartialAction[Expr, Relabeling] = new VecRelabelingAction[Expr]
  implicit val CorrRelabelingAction: PartialAction[Corr, Relabeling] = new VecRelabelingAction[Corr]
  implicit val ExprSymmetricForms: SymmetricForms[Expr] = new VecSymmetricForms[Expr]
  implicit val CorrSymmetricForms: SymmetricForms[Corr] = new VecSymmetricForms[Corr]
  implicit def ExprRepresentatives = new VecRepresentatives[Expr]
  implicit def CorrRepresentatives = new VecRepresentatives[Corr]
  implicit def ExprReprSyntax(expr: Expr) = new VecRepresentativesSyntax(expr)
  implicit def CorrReprSyntax(corr: Corr) = new VecRepresentativesSyntax(corr)
  implicit val ExprRelabelingExtractor = new VecRelabelingExtractor[Expr]
  implicit val CorrRelabelingExtractor = new VecRelabelingExtractor[Corr]
}
*/