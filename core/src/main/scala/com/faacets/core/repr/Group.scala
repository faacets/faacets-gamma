package com.faacets
package core
package repr

import spire.algebra.Eq

import com.faacets.data.Textable
/*
case class Group(n: Int, plus: Set[Int], minus: Set[Int]) {

  override def toString =
    (if (!isMaxInSets) n + ":" else "") + (plus.toSeq.sorted.mkString(",") + "/" + minus.toSeq.sorted.mkString(","))

  def isMarginal = plus == Set(0 until n: _*) && minus.isEmpty

  def isBinary = (n == 2)

  def index = Q(n).groups.zipWithIndex.find(_._1 == this).getOrElse(throw new IllegalArgumentException("Invalid group " + toString))._2

  def isMaxInSets = (plus ++ minus).max == (n - 1)

}

object Group {

  def marginal(n: Int): Group = Group(n, Set(0 until n:_*), Set.empty[Int])

  def binary = Group(2, Set(0), Set(1))

  def fromIndex(n: Int, index: Int): Group = Q(n).group(index)

  implicit val equ: Eq[Group] = spire.optional.genericEq.generic[Group]

  implicit val textable: Textable[Group] = Textable.fromParser[Group](Parsers.group, _.toString)

}*/
