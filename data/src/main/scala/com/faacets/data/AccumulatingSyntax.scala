package com.faacets.data

import io.circe._

object AccumulatingSyntax {

  implicit class DataJsonOps(val lhs: Json) extends AnyVal {

    def asAcc[A](implicit d: AccumulatingDecoder[A]): AccumulatingDecoder.Result[A] = d(lhs.hcursor)

  }

}
