package com.faacets.operation

import com.faacets.operation.lifting.GroupingParsers
import fastparse.noApi._
import spire.math.Rational

object Parsers {

  import com.faacets.data.Parsers.{rational, White, nonNegativeRational}
  import com.faacets.core.Parsers.scenario

  import White._

  import GroupingParsers.grouping

  val lifting: P[Lifting] = P( grouping ~ "->" ~ grouping ).flatMap {
    case (source, target) => Lifting.validate(source, target).fold(s => Fail.opaque(s.toList.mkString(",")), l => Pass.map(x => l))
  }

  val reordering: P[Reordering] = P( scenario ~ "->" ~ scenario ).map { case (s, t) => Reordering(s, t) }

  val idmultpart: P[Rational] = P("x").map(str => Rational.one)

  val minusmultpart: P[Rational] = P("-" ~ "x").map(str => -Rational.one)

  val explicitmultpart: P[Rational] = P(rational ~ ("*".? ~ "x"))

  val multpart: P[Rational] = P(explicitmultpart | minusmultpart | idmultpart)

  val sign: P[Rational] = P("+").map(x => Rational.one) | P("-").map(x => -Rational.one)

  val rationalCoefficientForceSign: P[Rational] = P(sign ~ nonNegativeRational).map { case (s, r) => s * r }

  val shiftpart: P[Rational] = rationalCoefficientForceSign.?.map {
    case Some(rat) => rat
    case None => Rational.zero
  }

  val affine: P[Affine] = P(multpart ~ shiftpart).map {
    case (a, b) => Affine(a, b)
  }

  val positiveInt: P[Int] = P( (CharIn('1'to'9') ~~ CharIn('0'to'9').repX).! ).map(_.toInt)

  val part: P[Set[Int]] = P( CharIn('A'to'Z').repX(min=1).! ).flatMap { str =>
    val set = str.map(_ - 'A').toSet
    if (set.size == str.size) Pass.map(x => set) else Fail
  }

  val product: P[Set[Set[Int]]] = part.repX(min=1, sep="x").flatMap { sets =>
    val all = sets.flatten.toSet
    if (all.size == sets.foldLeft(0)( (i, s) => i + s.size)) Pass.map(x => sets.toSet) else Fail
  }

  val onlyConstant: P[(Set[Set[Int]], Rational)] = nonNegativeRational.map( r => (Set.empty[Set[Int]], r) )

  val onlyProduct: P[(Set[Set[Int]], Rational)] = product.map( p => (p, Rational.one) )

  val constantProduct: P[(Set[Set[Int]], Rational)] = P(nonNegativeRational ~ product).map { case (r, t) => (t, r) }

  val nnTerm: P[(Set[Set[Int]], Rational)] = P( constantProduct | onlyProduct | onlyConstant )

  val firstTerm: P[(Set[Set[Int]], Rational)] = P(sign.? ~ nnTerm).map {
    case (Some(s), (p, c)) => (p, s*c)
    case (None, term) => term
  }

  val tailTerm: P[(Set[Set[Int]], Rational)] =  P(sign ~ nnTerm).map {
    case (s, (p, c)) => (p, s*c)
  }

  val polyExpr: P[PolyExpr] = P(firstTerm ~ tailTerm.rep).map {
    case (h1, h2, t) => PolyExpr(((h1, h2) +: t).groupBy(_._1).mapValues(_.map(_._2).reduce(_ + _)).toMap)
  }

}
