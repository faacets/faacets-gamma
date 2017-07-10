package com.faacets.comp

import cats.{Applicative, Monad}
import cats.data.ValidatedNel
import com.faacets.consolidate.Result
import com.faacets.consolidate.syntax.all._
import com.faacets.core.{Expr, Party, Scenario}
import com.faacets.data.Value
import com.faacets.operation._
import cats.instances.all._
import cats.syntax.all._
import com.faacets.comp.ReadCompendiumTest.findCanonical
import cyclo.RealCyclo
import scalin.immutable.Vec
import spire.math.Rational
import com.faacets.operation.instances.all._

import scala.collection.immutable.{ListMap, ListSet}

trait ReadCompendiumAlg[F[_]] {

  implicit def F: Applicative[F]

  /** Returns, if it exists, the BellExpression in the database corresponding to the given Expr. */
  def findCanonical(expr: Expr): F[Option[BellExpression]]

  /** Returns the BellExpression, or if it does not exist a bare BellExpression. */
  def findCanonicalOrBare(expr: Expr): F[BellExpression] =
    findCanonical(expr).map(_.getOrElse(BellExpression.fromExpr(expr)))

}

object ReadCompendiumTest extends ReadCompendiumAlg[cats.Id] {

  def F = Applicative[cats.Id]

  val canonicalPositivity = BellExpression(
    BoundedExpr.canonicalPositivity,
    None,
    ListSet.empty[String],
    Some("Positivity"),
    ListSet("Positivity inequality"),
    ListMap.empty[String, ListSet[String]]
  )

  val canonicalCHSH = BellExpression(
    BoundedExpr.canonicalCHSH,
    None,
    ListSet.empty[String],
    Some("CHSH"),
    ListSet("Clauser-Horne-Shimony-Holt inequality"),
    ListMap("upper.bounds.local" -> ListSet("doi:10.1103/PhysRevLett.23.880"),
      "upper.bounds.quantum" -> ListSet("doi:10.1007/BF00417500", "http://www.tau.ac.il/~tsirel/download/qbell80.html")
    )
  )

  val database: Map[Expr, BellExpression] = Seq(canonicalPositivity, canonicalCHSH).map( be => (be.expr -> be) ).toMap


  def findCanonical(expr: Expr) = database.get(expr)

}

class CompleteBellExpression[F[_]: Monad](val rc: ReadCompendiumAlg[F]) {

  import rc._

  def completeBellExpression(be: BellExpression): F[Result[BellExpression]] = {
    val decExpr = be.decomposition
    // canonical components in the decomposition
    val exprs = decExpr.elements.map(_.canonical).toSet
    // either construct a bare BellExpression from each Expr, or get a value

    val vec: F[Vector[(Expr, BellExpression)]] =
      exprs.toVector.map(expr => findCanonicalOrBare(expr).map(expr -> _)).sequenceU
    val canonicalMapF: F[Map[Expr, BellExpression]] = vec.map(_.toMap)

    canonicalMapF.map { canonicalMap =>
      val decBellExpr = decExpr.map(_.map(canonicalMap))
      val reconstruct = decBellExpr.toProductTreeOption.fold(decBellExpr.map(_.original).original)(_.map(_.original).original)
      be merge reconstruct
    }
  }

}
