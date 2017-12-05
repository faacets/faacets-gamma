package com.faacets
package core
package repr

import spire.syntax.cfor._
import scalin.immutable.Mat
import scalin.immutable.dense._
import scalin.syntax.all._

/** Basis for probability distributions
  * 
  * Probability distributions \\( P(i), i = 1...n \\) are normalized,
  * and as such obey the following constraint:
  * \\[ \sum_i P(i) = 1 \\].
  * 
  * Thus, we define a basis \\( \{ \vec{P}_k \} \\) for \\( \vec{P} \\) (seen here
  * as a vector using a suitable enumeration of its coefficients) such that:
  * \\[ \vec{P} = \vec{P}_0 + \sum_k p_k \vec{P}_k \\]
  * and
  * \\[ \vec{P}_0 = \frac{1}{n} \begin{pmatrix} 1 & 1 & ... & 1 \end{pmatrix} \\].
  * 
  * The matrix `Q` defines this conversion.
  */
object Q extends UniquenessCache[Int, Q] {
  override def apply(n: Int): Q = super.apply(n)
  protected def valueFromKey(n: Int): Q = new Q(n)
  protected def keyFromValue(q: Q): Option[Int] = Some(q.n)
}

final class Q(val n: Int) {
  import spire.math.Rational

  lazy val matrix: Mat[Rational] = Mat.fromMutable(n, n, Rational.zero) { Mp =>
    Mp(0, ::) := 1
    Mp(1 until n, n - 1) := -1
    cforRange(1 until n) { r =>
      cforRange(0 until n - 1) { c =>
        if (r - 1 == c)
          Mp(r, c) := n - 1
        else
          Mp(r, c) := -1
      }
    }
  }

  lazy val matrixInverse: Mat[Rational] = Mat.fromMutable(n ,n ,Rational.zero) { Ip =>
    val plus = Rational(1, n)
    val minus = Rational(-1, n)
    Ip(::,0) := plus
    Ip(n - 1, 1 until n) := minus
    cforRange(0 until n - 1) { r =>
      Ip(r, r + 1) := plus
    }
  }
/*
  def group(i: Int): Group = {
    val plus = (0 until n).filter(matrix(i, _) == 1).toSet
    val minus = (0 until n).filter(matrix(i, _) == -1).toSet
    Group(n, plus, minus)
  }

  def groups = (0 until n).map(group)*/

}
