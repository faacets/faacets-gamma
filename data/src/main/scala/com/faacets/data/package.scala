package com.faacets

import cats.data.{NonEmptyList => Nel, Validated, ValidatedNel}
import io.circe._

package object data {

  implicit class RichValidatedNel[A](val lhs: ValidatedNel[String, A]) extends AnyVal {

    def toAccumulatingDecoderResult: AccumulatingDecoder.Result[A] =
      lhs.leftMap(errors => errors.map(error => DecodingFailure(error, Nil)))

  }

}
