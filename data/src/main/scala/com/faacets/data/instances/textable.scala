package com.faacets.data
package instances

import io.circe.{AccumulatingDecoder, Decoder, Encoder, HCursor}
import com.faacets.data.syntax.validatedNel._
import io.circe.Decoder.Result

trait TextableInstances {

  implicit def textableEncoder[T](implicit ev: Textable[T]): Encoder[T] = Encoder.encodeString.contramap[T](Textable[T].toText(_))

  implicit def textableDecoder[T](implicit ev: Textable[T]): Decoder[T] = new Decoder[T] {

    def apply(c: HCursor): Result[T] = decodeAccumulating(c).leftMap(_.head).toEither

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[T] =
      ForcedStringDecoder.accumulating(c) andThen ( str => Textable[T].fromText(str).toAccumulatingDecoderResult )

  }
  /*
    Decoder.instance[T] { cursor =>
    ForcedStringDecoder(cursor).andThen( str => Textable[T].fromText(str).toAccumulatingDecoderResult )
  }*/

}
