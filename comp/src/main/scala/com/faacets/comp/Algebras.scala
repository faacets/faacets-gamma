package com.faacets.comp

import scala.collection.immutable.{ListMap, ListSet}

import cats.instances.all._
import cats.syntax.all._
import cats.{Applicative, Monad}

import com.faacets.consolidate.Result
import com.faacets.consolidate.syntax.all._
import com.faacets.core.Expr
import com.faacets.operation._
import com.faacets.operation.instances.all._

trait ReadCompendiumAlg[F[_]] {

  implicit def F: Applicative[F]

  /** Returns, if it exists, the BellExpression in the database corresponding to the given Expr. */
  def findCanonical(expr: Expr): F[Option[BellExpression]]

  /** Returns the BellExpression, or if it does not exist a bare BellExpression. */
  def findCanonicalOrBare(expr: Expr): F[BellExpression] =
    findCanonical(expr).map(_.getOrElse(BellExpression.fromExpr(expr)))

  /** Returns the bound rules in that compendium. */
  def boundRules: F[BoundRules]

}

object ReadCompendiumTest extends ReadCompendiumAlg[cats.Id] {

  def F = Applicative[cats.Id]

  val canonicalPositivity = BellExpression(
    BoundedExpr.canonicalPositivity,
    None,
    Some("Positivity"),
    Some("Positivity inequality")
  )

  /*
      ListMap(
      List("upper","bounds","local") -> ListSet("doi:10.1103/PhysRevLett.23.880"),
      List("upper", "bounds","quantum") -> ListSet("doi:10.1007/BF00417500", "http://www.tau.ac.il/~tsirel/download/qbell80.html")
    )
   */
  val canonicalCHSH = BellExpression(
    BoundedExpr.canonicalCHSH,
    None,
    Some("CHSH"),
    Some("Clauser-Horne-Shimony-Holt inequality"),
  )

  val database: Map[Expr, BellExpression] = Seq(canonicalPositivity, canonicalCHSH).map( be => (be.expr -> be) ).toMap

  def findCanonical(expr: Expr) = database.get(expr)

  val boundRules = BoundRules.standardBoundRules

}

class CompleteBellExpression[F[_]: Monad](val rc: ReadCompendiumAlg[F]) {

  import rc._

  def completeBellExpression(be: BellExpression): F[Result[BellExpression]] =
    boundRules flatMap { implicit br =>

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
