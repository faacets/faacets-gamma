package com.faacets
package operation
package lifting

import fastparse.noApi._

object LiftingParsers {

  import com.faacets.data.Parsers._

  import White._

  import GroupingParsers.grouping

  val lifting: P[Lifting] = P( grouping ~ "->" ~ grouping ).flatMap {
    case (source, target) => Lifting.validate(source, target).fold(s => Fail.opaque(s.toList.mkString(",")), l => Pass.map(x => l))
  }

}
