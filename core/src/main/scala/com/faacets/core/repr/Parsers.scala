package com.faacets.core.repr

import fastparse.noApi._

object Parsers {

  import com.faacets.data.Parsers._
  import White._

  def index: P[Int] = nonNegativeInt // starting from 0

  def groupPlusMinus: P[(Int, Set[Int], Set[Int])] =
    P( (positiveInt ~ ":").? ~ index.rep(min=1,sep=",") ~ "/" ~ index.rep(min=1,sep=",") ).map {
      case (nOption, plusSeq, minusSeq) =>
        val n = nOption.getOrElse((plusSeq ++ minusSeq).max)
        ((n, plusSeq.toSet, minusSeq.toSet))
    }

  def group: P[Group] = groupPlusMinus.filter {
    case (n, plus, minus) => plus.forall(_ < n) && minus.forall(_ < n) && (plus intersect minus).isEmpty
  }.map { case (n, plus, minus) => Group(n, plus, minus) }

}
