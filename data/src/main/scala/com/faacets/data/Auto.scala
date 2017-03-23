package com.faacets.data

import cats.data.{NonEmptyList => NEL, Validated, ValidatedNel}
import cats.syntax.all._

import io.circe._

import shapeless._

object AutoAccumulatingDecoder extends LabelledTypeClassCompanion[AccumulatingDecoder] {

  class Want[T](val u: Unit) {

    def noValidation[LKV](implicit
      lgen: LabelledGeneric.Aux[T, LKV],
      lwclkv: Lazy[Wrap[LKV]]
    ): AccumulatingDecoder[T] = deriveInstance[T, LKV]

    def validated[LKV, V, G](validate: G => ValidatedNel[String, T])(implicit
      lgen: LabelledGeneric.Aux[T, LKV], // LKV is the labelled generic representation
      gen: Generic.Aux[G, V], // G is the unlabelled generic representation
      lwclkv: Lazy[Wrap.Aux[LKV, V]]
    ): AccumulatingDecoder[T] = {

      import lwclkv.value.{label, unlabel, unwrap}

      AccumulatingDecoder.instance { c =>
        unwrap(c) andThen (v => validate(gen.from(v)).toAccumulatingDecoderResult)
      }

    }

  }

  def derive[T]: Want[T] = new Want[T]( () )

  object typeClass extends LabelledTypeClass[AccumulatingDecoder] {
    def emptyProduct = Decoder.instance(c => Right(HNil: HNil)).accumulating

    def product[F, T <: HList](name: String, mh: AccumulatingDecoder[F], mt: AccumulatingDecoder[T]) =
      (mh product mt).map { case (x, y) => x :: y }

    def emptyCoproduct = Decoder.failedWithMessage("CNil").accumulating

    def coproduct[L, R <: Coproduct](name: String, sl: => AccumulatingDecoder[L], sr: => AccumulatingDecoder[R]) =
      AccumulatingDecoder.instance[L :+: R](c =>
        (sl(c), sr(c)) match {
          case (Validated.Valid(l), Validated.Invalid(_)) => Validated.valid(Coproduct[L :+: R](l))
          case (Validated.Invalid(_), Validated.Valid(r)) => Validated.valid(r.extendLeftBy[L :+: CNil])
          case (Validated.Valid(l), Validated.Valid(_)) =>
            Validated.invalidNel(DecodingFailure(s"Can parse as $name but also as something else", Nil))
          case (Validated.Invalid(_), Validated.Invalid(_)) =>
            Validated.invalidNel(DecodingFailure(s"Cannot parse as $name but neither other possibilites", Nil))
        }
      )

    def project[F, G](instance: => AccumulatingDecoder[G], to: F => G, from: G => F) =
      instance.map(from)

  }

}
/*
object AutoEncoder extends LabelledTypeClassCompanion[Encoder] {

}
 */
