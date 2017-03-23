package com.faacets
package data

import org.scalacheck.{Arbitrary, Prop}

import Arbitrary.arbitrary

import io.circe._

import spire.math.{Rational, SafeLong}

import scalin.immutable.Vec

import spire.algebra.Eq

/*
case class Person(name: String, age: Int /* TODO , extra: JsonObject = JsonObject.empty */) /* extends WithExtra*/

object Person {

  implicit val personEncodeJson: EncodeJson[Person] =
    EncodeJson[Person] { p =>
      jObject(p.extra.toList.foldLeft(JsonObject.fromTraversableOnce(Seq("name" -> p.name.asJson, "age" -> p.age.asJson))) { (json, jf) => json :+ jf })
    }

  implicit val personDecodeJson: DecodeJson[Person] =
    DecodeJson[Person] { a =>
      jdecode2L( (name: String, age: Int) => Person(name, age) )("name", "age").decode(a).map(_.copy(extra = a.focus.obj.getOrElse(JsonObject.empty) - "name" - "age"))
    }

}

class PersonSuite extends FaacetsSuite {

  test("Conversion to plain data and back") {
    val john = Person("John", 45)
    assert(john == john.asJson.jdecode[Person].fold((_,_) => sys.error("Bad result"), identity))
  }

}
*/

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

}
