package com.faacets.data.syntax

import cats.data.ValidatedNel

import io.circe.{AccumulatingDecoder, DecodingFailure}


final class ValidatedNelOps[A](val lhs: ValidatedNel[String, A]) extends AnyVal {

  def toAccumulatingDecoderResult: AccumulatingDecoder.Result[A] =
    lhs.leftMap(errors => errors.map(error => DecodingFailure(error, Nil)))

}

trait ValidatedNelSyntax {

  implicit def dataValidatedNel[A](lhs: ValidatedNel[String, A]): ValidatedNelOps[A] = new ValidatedNelOps[A](lhs)

}
