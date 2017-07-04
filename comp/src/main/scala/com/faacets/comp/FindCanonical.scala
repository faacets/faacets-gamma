package com.faacets.comp

import com.faacets.core.Expr

trait ReadCanonical[F[_]] {
  def findCanonical(expr: Expr): F[Option[BellExpression]]

}
