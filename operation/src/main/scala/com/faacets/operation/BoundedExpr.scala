package com.faacets.operation

import scala.collection.immutable.ListMap

import cats.data.{Validated, ValidatedNel}
import cats.kernel.Comparison
import spire.algebra.partial.{Groupoid, PartialAction}
import spire.math.Rational
import spire.syntax.groupoid._
import spire.syntax.partialAction._
import spire.util.Opt
import cyclo.RealCyclo
import scalin.immutable.Vec
import scalin.immutable.dense._
import net.alasc.finite.Grp
import net.alasc.perms.default._

import io.circe._
import io.circe.syntax._

import com.faacets.consolidate.syntax.all._
import com.faacets.consolidate.{Merge, Result}
import com.faacets.core._
import com.faacets.data.Value
import com.faacets.data.instances.all._
import com.faacets.data.syntax.all._
import com.faacets.operation.instances.relabeling._
//import com.faacets.operation.product.BoundedExprTensor

trait BoundedExpr { self =>
  type S <: Scenario with Singleton
  def expr: Expr[S]
  def lower: LowerOrientation
  def upper: UpperOrientation
  override def equals(a: Any): Boolean = a match {
    case that: BoundedExpr => (self.expr == that.expr) && (self.lower == that.lower) && (self.upper == that.upper)
  }
  override def toString: String = s"BoundedExpr($expr, $lower, $upper)"
  override def hashCode: Int = expr.hashCode + lower.hashCode * 41 + upper.hashCode * 41 * 41
}

object BoundedExpr {

  type Aux[S0 <: Scenario with Singleton] = BoundedExpr { type S = S0 }

  def apply[S0 <: Scenario with Singleton](expr0: Expr[S0], lower0: LowerOrientation = LowerOrientation.empty, upper0: UpperOrientation = UpperOrientation.empty): BoundedExpr.Aux[S0] =
    new BoundedExpr {
      type S = S0
      val expr: Expr[S] = expr0
      val lower: LowerOrientation = lower0
      val upper: UpperOrientation = upper0
    }

  def unapply(be: BoundedExpr): Option[(Expr[be.S], LowerOrientation, UpperOrientation)] =
    Some((be.expr, be.lower, be.upper))
}

  /*
  def decomposition(implicit br: BoundRules): PolyProduct[CanonicalDec[BoundedExpr[S]]] =
    ProductExtractor[BoundedExpr[S]].forceExtract(BoundedExpr(expr))
      .mapAffine(be => CanonicalWithAffineExtractor[BoundedExpr[S]].apply(be).splitAffine)

  def reconstructBounds(implicit br: BoundRules): BoundedExpr[S] = {
    val pprec = decomposition.map(_.map { be =>
      BoundedExpr.canonicals.get(be.expr) match {
        case Some(c) => c
        case None => be
      }
    })
    pprec.toProductTreeOption.fold(decomposition.map(_.original).original)(_.map(_.original).original)
  }
*/
/*
object BoundedExpr {

  val canonicalPositivity: BoundedExpr[Scenario._112.type] = BoundedExpr(
    Expr.canonicalPositivity,
    LowerOrientation(ListMap("local" -> Value(-1), "quantum" -> Value(-1), "nonsignaling" -> Value(-1)),
      ListMap("local" -> true, "nonsignaling" -> true)),
    UpperOrientation(ListMap("local" -> Value(1), "quantum" -> Value(1), "nonsignaling" -> Value(1)),
      ListMap("local" -> true, "nonsignaling" -> true))
  )

  val canonicalCHSH: BoundedExpr[Scenario.CHSH.type] = BoundedExpr(
    Expr.canonicalCHSH,
    LowerOrientation(ListMap("local" -> Value(-2), "quantum" -> Value(-RealCyclo.sqrt2*2), "nonsignaling" -> Value(-4)),
      ListMap("local" -> true)),
    UpperOrientation(ListMap("local" -> Value(2), "quantum" -> Value(RealCyclo.sqrt2*2), "nonsignaling" -> Value(4)),
      ListMap("local" -> true))
  )

  val CH: BoundedExpr[Scenario.CHSH.type] = BoundedExpr(
    Expr.collinsGisin(Scenario.CHSH, Vec[Rational](0,0,-1,-1,1,1,0,-1,1)),
    LowerOrientation.empty,
    UpperOrientation(ListMap("local" -> Value(0)), ListMap.empty[String, Boolean])
  )

  val stdPreserved = Set("local", "quantum", "nonsignaling")

  /*
  def constructPartialAction[O:Groupoid](boundsF: (String, Value) => Option[(String, Value)],
                                         facetOfF: (String, Boolean) => Option[(String, Boolean)])
                                        (implicit exprPA: PartialAction[Expr, O]): PartialAction[BoundedExpr, O] =
    new PartialAction[BoundedExpr, O] {

      def partialActr(be: BoundedExpr, o: O): Opt[BoundedExpr] = {
        val newLower = be.lower.processBounds(boundsF).processFacetOf(facetOfF)
        val newUpper = be.upper.processBounds(boundsF).processFacetOf(facetOfF)
        (be.expr <|+|? o) match {
          case Opt(newExpr) =>
            Opt(BoundedExpr(newExpr, newLower, newUpper))
          case _ => Opt.empty[BoundedExpr]
        }
      }

      def partialActl(o: O, be: BoundedExpr): Opt[BoundedExpr] = partialActr(be, o.inverse)

    }

  implicit def constructExtractor[O:Groupoid](implicit O: OperationExtractor[Expr, O],
                                     pa: PartialAction[BoundedExpr, O]): OperationExtractor[BoundedExpr, O] =
    new OperationExtractor[BoundedExpr, O] {
      def partialAction: PartialAction[BoundedExpr, O] = pa
      def groupoid: Groupoid[O] = implicitly
      def identity(be: BoundedExpr): O = O.identity(be.expr)
      def extractOperation(be: BoundedExpr): Opt[O] = O.extractOperation(be.expr)
    }

  implicit def tensor(implicit ppb: ProductPreservedBounds): Tensor[BoundedExpr] = new BoundedExprTensor

  def validate(expr: Expr, lower: LowerOrientation, upper: UpperOrientation): ValidatedNel[String, BoundedExpr] =
    Validated.Valid(BoundedExpr(expr, lower, upper))

  implicit lazy val encode: Encoder[BoundedExpr] = Encoder.instance[BoundedExpr] { be =>
    Json.obj(
      "scenario" -> be.expr.scenario.asJson,
      "coefficients" -> be.expr.coefficients.asJson,
      "symmetryGroup" -> NDVec.attributes.symmetryGroup.get(be.expr).fold(Json.Null)(_.asJson),
      "lower" -> (if (be.lower.isEmpty) Json.Null else be.lower.asJson),
      "upper" -> (if (be.upper.isEmpty) Json.Null else be.upper.asJson)
    )
  }

  implicit lazy val decode: Decoder[BoundedExpr] = new Decoder[BoundedExpr] {
    def apply(c: HCursor): Decoder.Result[BoundedExpr] = decodeAccumulating(c).leftMap(_.head).toEither

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[BoundedExpr] =
      AccumulatingDecoder.resultInstance.map5(
        Decoder[Scenario].tryDecodeAccumulating(c.downField("scenario")),
        Decoder[Vec[Rational]].tryDecodeAccumulating(c.downField("coefficients")),
        Decoder[Option[Grp[Relabeling]]].tryDecodeAccumulating(c.downField("symmetryGroup")),
        Decoder[Option[LowerOrientation]].tryDecodeAccumulating(c.downField("lower")).map(_.getOrElse(LowerOrientation.empty)),
        Decoder[Option[UpperOrientation]].tryDecodeAccumulating(c.downField("upper")).map(_.getOrElse(UpperOrientation.empty))
      )( (_, _, _, _, _) ).andThen {
        case (s: Scenario, c: Vec[Rational], sg: Option[Grp[Relabeling]], lower: LowerOrientation, upper: UpperOrientation) =>
          Expr.validate(s, c, sg).toAccumulatingDecoderResult
            .andThen { expr => BoundedExpr.validate(expr, lower, upper).toAccumulatingDecoderResult }
      }
  }

  implicit lazy val merge: Merge[BoundedExpr] = new Merge[BoundedExpr] {

    def merge(base: BoundedExpr, newBE: BoundedExpr): Result[BoundedExpr] = {
      import cats.syntax.all._
      val expr = base.expr merge newBE.expr
      val lower = base.lower merge newBE.lower
      val upper = base.upper merge newBE.upper
      (expr |@| lower |@| upper) .map( (_,_,_) ).validate((BoundedExpr.validate _).tupled)
    }

  }

  /** Lexicographic order on BoundedExpr provided by the lexicographic order on Expr. */
  implicit val lexicographicOrder: LexicographicOrder[BoundedExpr] = new LexicographicOrder[BoundedExpr] {
    def partialComparison(x: BoundedExpr, y: BoundedExpr): Option[Comparison] =
      LexicographicOrder[Expr].partialComparison(x.expr, y.expr)
  }

  /** Addition of BoundedExpr. Does not recover bounds, TODO */
  implicit val additiveGroupoid: AdditiveGroupoid[BoundedExpr] = AdditiveGroupoid(new Groupoid[BoundedExpr] {
    def inverse(a: BoundedExpr): BoundedExpr = BoundedExpr(AdditiveGroupoid[Expr].groupoid.inverse(a.expr))
    def partialOp(x: BoundedExpr, y: BoundedExpr): Opt[BoundedExpr] = {
      AdditiveGroupoid[Expr].groupoid.partialOp(x.expr, y.expr) match {
        case Opt(r) => Opt(BoundedExpr(r))
        case _ => Opt.empty[BoundedExpr]
      }
    }
  })
*/
}
*/