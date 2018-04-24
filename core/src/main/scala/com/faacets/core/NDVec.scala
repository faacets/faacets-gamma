package com.faacets.core

import cats.data.{Validated, ValidatedNel}
import spire.algebra.Eq
import spire.math.Rational
import scalin.immutable.Vec
import net.alasc.attributes.Attributes
import net.alasc.bsgs.FixingPartition
import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.perms.default._

import io.circe._
import io.circe.syntax._

import com.faacets.consolidate.instances.all._
import com.faacets.consolidate.syntax.all._
import com.faacets.consolidate.{Merge, Result}
import com.faacets.data.instances.all._
import com.faacets.data.syntax.all._

abstract class NDVec[V[X <: Scenario with Singleton] <: NDVec[V, X], S <: Scenario with Singleton] extends PVec[V, S] { lhs: V[S] =>

  def symmetryGroup: Grp[Relabeling.Aux[S]] = NDVec.attributes.symmetryGroup(this: NDVec[V, S]) {
    val partition = Partition.fromSeq(coefficients.toIndexedSeq)
    (scenario: S).group.fixingPartition((scenario: S).probabilityAction, partition)
  }

}

object NDVec {

  object attributes extends Attributes("NDVec") {

    object symmetryGroup extends Attribute("symmetryGroup") {
      implicit def forNDVec[V[X <: Scenario with Singleton] <: NDVec[V, X], S <: Scenario with Singleton]: For[NDVec[V, S], Grp[Relabeling.Aux[S]]] = For
    }

  }

}

trait NDVecBuilder[V[X <: Scenario with Singleton] <: NDVec[V, X]] extends PVecBuilder[V] { self =>

  protected[faacets] def updatedWithSymmetryGroup[S <: Scenario with Singleton](original: V[S], newCoefficients: Vec[Rational],
                                                  symGroupF: (Grp[Relabeling.Aux[S]]) => Option[Grp[Relabeling.Aux[S]]]): V[S] = {
    val res = apply(original.scenario: S, newCoefficients)
    NDVec.attributes.symmetryGroup.get(original).flatMap(symGroupF) match {
      case Some(newGrp) => NDVec.attributes.symmetryGroup(res)(newGrp)
      case None => // we do not have an updated group
    }
    res
  }

  def inNonSignalingSubspace(scenario: Scenario, coefficients: Vec[Rational]): Boolean

  def validate[S <: Scenario with Singleton](scenario: S, coefficients: Vec[Rational], symGroup: Option[Grp[Relabeling.Aux[S]]] = None): ValidatedNel[String, V[S]] = {
    val correctLength = scenario.shapeP.size
    val coeffLength = coefficients.length
    if (coeffLength != correctLength) Validated.invalidNel(s"Invalid coefficients length, is $coeffLength, should be $correctLength")
    else if (!inNonSignalingSubspace(scenario, coefficients)) Validated.invalidNel("Coefficients are not in the nonsignaling subspace")
    else {
      val res = applyUnsafe(scenario: S, coefficients)
      symGroup match {
        case Some(grp) =>
          val partition = Partition.fromSeq(coefficients.toIndexedSeq)
          grp.generators.find(!FixingPartition.partitionInvariantUnder(partition, (scenario: S).probabilityAction, _)) match {
            case Some(g) =>
              Validated.invalidNel(s"Coefficients are not invariant under provided generator $g")
            case None =>
              NDVec.attributes.symmetryGroup(res)(grp)
              Validated.valid(res)
          }
        case None =>
          Validated.valid(res)
      }
    }
  }

  /*
  implicit lazy val merge: Merge[V] = new Merge[V] {

    def merge(base: V, newV: V): Result[V] = {
      import cats.syntax.all._

      import NDVec.attributes.{symmetryGroup => sg}
      val scenario = base.scenario merge newV.scenario
      val coefficients = base.coefficients merge newV.coefficients
      val symGroup = sg.get(base) merge sg.get(newV)
      (scenario |@| coefficients |@| symGroup).map( (_,_,_) ).validate((validate _).tupled)
    }

  }

  implicit val encode: Encoder[V] = Encoder.instance[V] { v =>
    Json.obj(
      "scenario" -> v.scenario.asJson,
      "coefficients" -> v.coefficients.asJson,
      "symmetryGroup" -> NDVec.attributes.symmetryGroup.get(v).fold(Json.Null)(_.asJson)
    )
  }

  implicit val decode: Decoder[V] = new Decoder[V] {

    def apply(c: HCursor): Decoder.Result[V] = decodeAccumulating(c).leftMap(_.head).toEither

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[V] =
      AccumulatingDecoder.resultInstance.map3(
        Decoder[Scenario].tryDecodeAccumulating(c.downField("scenario")),
        Decoder[Vec[Rational]].tryDecodeAccumulating(c.downField("coefficients")),
        Decoder[Option[Grp[Relabeling]]].tryDecodeAccumulating(c.downField("symmetryGroup"))
      )( (_, _, _) ).andThen {
        case (s: Scenario, c: Vec[Rational], sg: Option[Grp[Relabeling]]) => self.validate(s, c, sg).toAccumulatingDecoderResult
      }

  }*/

}
