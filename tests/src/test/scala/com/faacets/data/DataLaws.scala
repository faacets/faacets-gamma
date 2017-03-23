package com.faacets.data

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import spire.algebra.Eq
import spire.syntax.eq._

import cats.data.{NonEmptyList => Nel, Validated}

import io.circe._
import io.circe.yaml.parser
import io.circe.yaml.syntax._

import Textable.syntax._
import AccumulatingSyntax._

object DataLaws {

  def apply[A : Eq : Arbitrary] = new DataLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }

}

trait DataLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def coded(implicit d: AccumulatingDecoder[A], e: Encoder[A]) =
    new SimpleRuleSet(
      "coded",

      "through JSON AST" → forAll( (a: A) =>
        AccumulatingDecoder[A].apply(HCursor.fromJson(Encoder[A].apply(a))).fold( _ => false, _ === a)
      ),

      "through YAML" → forAll { (a: A) =>
        import cats.syntax.all._
        import cats.instances.all._

        val yml = Encoder[A].apply(a).asYaml.spaces2
        val res = Validated.fromEither(parser.parse(yml))
          .leftMap(x => Nel.of(x))
          .andThen(_.asAcc[A])

        res === Validated.valid(a)
      }
    )

  def textable(implicit ev: Textable[A]) =
    new SimpleRuleSet(
      "textable",

      "a.toText.fromText[A] === a" → forAll( (a: A) =>
        a.toText.parseUnsafe[A] === a
      )

    )

}
