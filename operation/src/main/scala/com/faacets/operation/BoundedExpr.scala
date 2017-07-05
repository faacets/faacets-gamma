package com.faacets.operation

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.kernel.Comparison
import com.faacets.consolidate.{Merge, Result}
import com.faacets.core._
import com.faacets.data.Value
import com.faacets.data.instances.all._
import com.faacets.operation.instances.relabeling._
import com.faacets.data.syntax.all._
import io.circe._
import io.circe.syntax._
import com.faacets.consolidate.instances.all._
import com.faacets.operation.product.BoundedExprTensor
import cyclo.RealCyclo
import net.alasc.perms.default._
import net.alasc.finite.Grp
import scalin.immutable.Vec
import spire.algebra.{Action, Group}
import spire.algebra.partial.{Groupoid, PartialAction}
import spire.math.Rational
import spire.syntax.partialAction._
import spire.syntax.action._
import spire.syntax.groupoid._
import com.faacets.consolidate.syntax.all._
import spire.syntax.group._
import spire.math.interval.Overlap
import spire.util.Opt
import scalin.immutable.dense._

import scala.collection.immutable.{ListMap, ListSet}

case class BoundedExpr(expr: Expr,
                       lower: LowerOrientation = LowerOrientation.empty,
                       upper: UpperOrientation = UpperOrientation.empty
                         ) {

  def decomposition: PolyProduct[CanonicalDec[BoundedExpr]] =
    ProductExtractor[BoundedExpr].forceExtract(BoundedExpr(expr))
      .mapAffine(be => CanonicalWithAffineExtractor[BoundedExpr].apply(be).splitAffine)

  def reconstructBounds: BoundedExpr = {
    val pprec = decomposition.map(_.map { be =>
      BoundedExpr.canonicals.get(be.expr) match {
        case Some(c) => c
        case None => be
      }
    })
    pprec.toProductTreeOption.fold(decomposition.map(_.original).original)(_.map(_.original).original)
  }

}

object BoundedExpr {

  val canonicalPositivity = BoundedExpr(
    Expr.canonicalPositivity,
    LowerOrientation(ListMap("local" -> Value(-1), "quantum" -> Value(-1), "nonsignaling" -> Value(-1)),
      ListMap("local" -> true, "nonsignaling" -> true)),
    UpperOrientation(ListMap("local" -> Value(1), "quantum" -> Value(1), "nonsignaling" -> Value(1)),
      ListMap("local" -> true, "nonsignaling" -> true))
  )

  val canonicalCHSH = BoundedExpr(
    Expr.canonicalCHSH,
    LowerOrientation(ListMap("local" -> Value(-2), "quantum" -> Value(-RealCyclo.sqrt2*2), "nonsignaling" -> Value(-4)),
      ListMap("local" -> true)),
    UpperOrientation(ListMap("local" -> Value(2), "quantum" -> Value(RealCyclo.sqrt2*2), "nonsignaling" -> Value(4)),
      ListMap("local" -> true))
  )

  val canonicals: Map[Expr, BoundedExpr] = Map(canonicalPositivity.expr -> canonicalPositivity, canonicalCHSH.expr -> canonicalCHSH)

  val CH = BoundedExpr(
    Expr.collinsGisin(Scenario.CHSH, Vec[Rational](0,0,-1,-1,1,1,0,-1,1)),
    LowerOrientation.empty,
    UpperOrientation(ListMap("local" -> Value(0)), ListMap.empty[String, Boolean])
  )

  val stdPreserved = Set("local", "quantum", "nonsignaling")

  def constructPartialAction[O:Groupoid](preservedBoundsAndFacetOf: Set[String])
                                 (implicit exprPA: PartialAction[Expr, O],
                                  valueA: Action[Value, O]): PartialAction[BoundedExpr, O] =
    new PartialAction[BoundedExpr, O] {

      def partialActr(be: BoundedExpr, o: O): Opt[BoundedExpr] = {
        def valueF(v: Value): Value = v <|+| o
        val newLower = be.lower.filterBoundsAndFacetOf(preservedBoundsAndFacetOf).mapBounds(valueF)
        val newUpper = be.upper.filterBoundsAndFacetOf(preservedBoundsAndFacetOf).mapBounds(valueF)
        (be.expr <|+|? o) match {
          case Opt(newExpr) =>
            Opt(BoundedExpr(newExpr, newLower, newUpper))
          case _ => Opt.empty[BoundedExpr]
        }
      }

      def partialActl(o: O, be: BoundedExpr): Opt[BoundedExpr] = partialActr(be, o.inverse)

    }

  def constructAction[O:Group](preservedBoundsAndFacetOf: Set[String])
                                        (implicit exprA: Action[Expr, O],
                                         valueA: Action[Value, O]): Action[BoundedExpr, O] =
    new Action[BoundedExpr, O] {

      def actr(be: BoundedExpr, o: O): BoundedExpr = {
        def valueF(v: Value): Value = v <|+| o
        val newLower = be.lower.filterBoundsAndFacetOf(preservedBoundsAndFacetOf).mapBounds(valueF)
        val newUpper = be.upper.filterBoundsAndFacetOf(preservedBoundsAndFacetOf).mapBounds(valueF)
        val newExpr = be.expr <|+| o
        BoundedExpr(newExpr, newLower, newUpper)
      }

      def actl(o: O, be: BoundedExpr): BoundedExpr = actr(be, o.inverse)

    }

  implicit def constructExtractor[O:Groupoid](implicit O: OperationExtractor[Expr, O],
                                     pa: PartialAction[BoundedExpr, O]): OperationExtractor[BoundedExpr, O] =
    new OperationExtractor[BoundedExpr, O] {
      def partialAction: PartialAction[BoundedExpr, O] = pa
      def groupoid: Groupoid[O] = implicitly
      def identity(be: BoundedExpr): O = O.identity(be.expr)
      def extractOperation(be: BoundedExpr): Opt[O] = O.extractOperation(be.expr)
    }

  implicit val tensor: Tensor[BoundedExpr] = new BoundedExprTensor

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
      import NDVec.attributes.{symmetryGroup => sg}
      val expr = base.expr merge newBE.expr
      val lower = base.lower merge newBE.lower
      val upper = base.upper merge newBE.upper
      (expr |@| lower |@| upper) .map( (_,_,_) ).validate((BoundedExpr.validate _).tupled)
    }

  }


  implicit val lexicographicOrder: LexicographicOrder[BoundedExpr] = new LexicographicOrder[BoundedExpr] {
    def partialComparison(x: BoundedExpr, y: BoundedExpr): Option[Comparison] =
      LexicographicOrder[Expr].partialComparison(x.expr, y.expr)
  }

  implicit val additiveGroupoid: AdditiveGroupoid[BoundedExpr] = AdditiveGroupoid(new Groupoid[BoundedExpr] {
    def inverse(a: BoundedExpr): BoundedExpr = BoundedExpr(AdditiveGroupoid[Expr].groupoid.inverse(a.expr))
    def partialOp(x: BoundedExpr, y: BoundedExpr): Opt[BoundedExpr] = {
      AdditiveGroupoid[Expr].groupoid.partialOp(x.expr, y.expr) match {
        case Opt(r) => Opt(BoundedExpr(r))
        case _ => Opt.empty[BoundedExpr]
      }
    }
  })

}
