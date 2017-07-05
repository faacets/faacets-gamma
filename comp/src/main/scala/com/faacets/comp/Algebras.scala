package com.faacets.comp

import cats.Monad
import cats.data.ValidatedNel
import com.faacets.consolidate.Result
import com.faacets.core.{Expr, Party, Scenario}
import com.faacets.data.Value
import com.faacets.operation._
import cyclo.RealCyclo
import scalin.immutable.Vec
import spire.math.Rational

import scala.collection.immutable.{ListMap, ListSet}

trait ReadCompendiumAlg[F[_]] {

  def findCanonical(expr: Expr): F[Option[BellExpression]]

}

object ReadCompendiumTest extends ReadCompendiumAlg[cats.Id] {

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

class CompleteBellExpression[F[_]: Monad](rc: ReadCompendiumAlg[F]) {

  /*
  def completeBellExpression(be: BellExpression): F[Result[BellExpression]] = {
    def decomposition: PolyProduct[CanonicalDec[BoundedExpr]] =
      ProductExtractor[BoundedExpr].forceExtract(BoundedExpr(be.expr))
        .mapAffine(be => CanonicalWithAffineExtractor[BoundedExpr].apply(be).splitAffine)

    decomposition.map(_.map { })
    def reconstructBounds: BoundedExpr = {
      val pprec = decomposition.map(_.map { be =>
        BoundedExpr.canonicals.get(be.expr) match {
          case Some(c) => c
          case None => be
        }
      })
      pprec.toProductTreeOption.fold(decomposition.map(_.original).original)(_.map(_.original).original)
    }

  }*/

}
