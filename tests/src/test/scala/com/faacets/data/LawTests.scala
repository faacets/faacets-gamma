package com.faacets
package data

import org.scalacheck.Arbitrary
import Arbitrary.arbitrary
import spire.math.{Rational, SafeLong}

import scalin.immutable.Vec
import spire.algebra.Eq
import com.faacets.laws.DataLaws

class LawTests extends FaacetsSuite {

  import spire.laws.arb.{safeLong => arbSafeLong, rational => arbRational}
  import net.alasc.perms.Perm
  import net.alasc.laws.Permutations._
  import com.faacets.data.instances.all._
  import scalin.immutable.dense._

  implicit def vecEq: Eq[Vec[Rational]] = Eq.fromUniversalEquals[Vec[Rational]]

  implicit val arbVecRational: Arbitrary[Vec[Rational]] =
    Arbitrary { arbitrary[IndexedSeq[Rational]].map(Vec.fromSeq(_)) }


  checkAll("DataLaws[Perm].textable", DataLaws[Perm].textable)

  checkAll("DataLaws[Rational].coded", DataLaws[Rational].coded)

  checkAll("DataLaws[SafeLong].coded", DataLaws[SafeLong].coded)

  checkAll("DataLaws[Vec[Rational]].coded", DataLaws[Vec[Rational]].coded)

  checkAll("DataLaws[Rational].textable", DataLaws[Rational].textable)

  checkAll("DataLaws[SafeLong].textable", DataLaws[SafeLong].textable)

}
