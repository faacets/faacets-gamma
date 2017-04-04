package com.faacets.core.text

import com.faacets.data.Textable
import spire.math.Rational
import scalin.immutable.Vec
import scalin.immutable.dense._

object UserVecRational {

  import com.faacets.data.Parsers._
  import White._
  import fastparse.noApi._

  val singleSpace: P[Unit] = CharIn(" \n\t\r")
  val commaSep: P[Unit] = singleSpace.repX ~~ "," ~~ singleSpace.repX
  val spaceSep: P[Unit] = singleSpace.repX(min = 1)
  val separator: P[Unit] = commaSep | spaceSep
  val nakedVec: P[Vec[Rational]] = P( rational.repX(min = 1, sep = separator).map(seq => Vec(seq: _*)) )
  val enclosedSquare: P[Vec[Rational]] = P( "[" ~ vec ~ "]" )
  val enclosedCurly: P[Vec[Rational]] = P( "{" ~ vec ~ "}" )
  val vec: P[Vec[Rational]] = P( enclosedCurly | enclosedSquare | nakedVec )

  implicit def userVecRationalTextable: Textable[Vec[Rational]] = Textable.fromParser(vec, _.toIndexedSeq.mkString("[",",","]"))

}
