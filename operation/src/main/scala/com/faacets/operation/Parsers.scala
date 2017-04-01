package com.faacets.operation

import com.faacets.operation.lifting.GroupingParsers
import GroupingParsers.grouping

import fastparse.noApi._

object Parsers {

  import com.faacets.data.Parsers.White
  import com.faacets.core.Parsers.scenario

  import White._

  import GroupingParsers.grouping

  val lifting: P[Lifting] = P( grouping ~ "->" ~ grouping ).flatMap {
    case (source, target) => Lifting.validate(source, target).fold(s => Fail.opaque(s.toList.mkString(",")), l => Pass.map(x => l))
  }

  val reordering: P[Reordering] = P( scenario ~ "->" ~ scenario ).map { case (s, t) => Reordering(s, t) }

}
