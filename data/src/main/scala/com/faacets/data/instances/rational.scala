package com.faacets.data
package instances

import scala.util.Try

import spire.math.Rational

import io.circe._

import com.faacets.consolidate.Merge

trait RationalInstances {
  import scala.util.{Left, Right}

  import Decoder.Result

  implicit val rationalEncoder: Encoder[Rational] = new Encoder[Rational] {
    def apply(r: Rational) =
      if (r.isWhole && r.numerator.isValidInt) Json.fromInt(r.numerator.toInt) else Json.fromString(r.toString)
  }

  implicit val rationalDecoder: Decoder[Rational] = new Decoder[Rational] {
    def apply(c: HCursor): Result[Rational] = {
      def failure(err: String): Either[DecodingFailure, Rational] = Left(DecodingFailure(err, c.history))
      def decodeNumber(jsn: JsonNumber): Either[DecodingFailure, Rational] = jsn.toBigInt.fold(failure(s"Number $jsn does not represent an exact rational number"))(bi => Right(Rational(bi)))
      def decodeString(str: String): Either[DecodingFailure, Rational] =
        Try(Rational(str))
          .toOption
          .fold(failure(s"String $str does not represent a rational number"))(Right(_))

      c.value.fold(
        failure("Rational should not be null"), // null
        x => failure("Rational should not be bool"), // bool
        decodeNumber, // number
        decodeString, // string
        x => failure("Rational should not be array"), // array
        x => failure("Rational should not be object") // object
      )
    }
  }

  implicit val rationalMerge: Merge[Rational] = Merge.fromEquals[Rational]

  implicit val rationalTextable: Textable[Rational] = Textable.fromParser(Parsers.rational, _.toString)

}
