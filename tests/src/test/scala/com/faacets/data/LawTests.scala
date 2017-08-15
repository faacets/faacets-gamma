package com.faacets
package data

import spire.algebra.Eq
import spire.math.{Rational, SafeLong}
import cyclo.RealCyclo
import scalin.immutable.Vec

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import com.faacets.laws.RealCyclos.arbRealCyclo
import com.faacets.laws.DataLaws

class LawTests extends FaacetsSuite {

  import spire.laws.arb.{rational => arbRational, safeLong => arbSafeLong}
  import scalin.immutable.dense._
  import net.alasc.laws.Permutations._
  import net.alasc.perms.Perm

  implicit def vecEq: Eq[Vec[Rational]] = Eq.fromUniversalEquals[Vec[Rational]]

  implicit val arbVecRational: Arbitrary[Vec[Rational]] =
    Arbitrary { arbitrary[IndexedSeq[Rational]].map(Vec.fromSeq(_)) }

  checkAll("DataLaws[Perm].textable", DataLaws[Perm].textable)

  checkAll("DataLaws[Rational].coded", DataLaws[Rational].coded)

  checkAll("DataLaws[SafeLong].coded", DataLaws[SafeLong].coded)

  checkAll("DataLaws[Vec[Rational]].coded", DataLaws[Vec[Rational]].coded)

  checkAll("DataLaws[Rational].textable", DataLaws[Rational].textable)

  checkAll("DataLaws[SafeLong].textable", DataLaws[SafeLong].textable)

  checkAll("DataLaws[RealCyclo].textable", DataLaws[RealCyclo].textable)

}
