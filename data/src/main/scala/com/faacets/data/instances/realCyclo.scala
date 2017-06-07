package com.faacets.data
package instances

import scala.util.Try
import spire.math.Rational
import com.faacets.consolidate.Merge
import cyclo.RealCyclo
import io.circe._

trait RealCycloInstances {
  import scala.util.{Left, Right}

  import Decoder.Result

  implicit val realCycloMerge: Merge[RealCyclo] = Merge.fromEquals[RealCyclo]

  implicit val realCycloTextable: Textable[RealCyclo] = Textable.fromParser(Parsers.realCyclo.realCyclo, _.toString)

}
