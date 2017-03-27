package com.faacets.data
package instances

import io.circe.{AccumulatingDecoder, Encoder}

import com.faacets.data.syntax.validatedNel._

trait TextableInstances {

  implicit def textableEncoder[T](implicit ev: Textable[T]): Encoder[T] = Encoder.encodeString.contramap[T](Textable[T].toText(_))

  implicit def textableDecoder[T](implicit ev: Textable[T]): AccumulatingDecoder[T] = AccumulatingDecoder.instance[T] { cursor =>
    ForcedStringDecoder.accumulating(cursor).andThen { str =>
      Textable[T].fromText(str).toAccumulatingDecoderResult
    }
  }

}