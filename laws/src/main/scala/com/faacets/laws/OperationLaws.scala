package com.faacets
package laws

import com.faacets.operation.OperationExtractor
import org.typelevel.discipline.Laws
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._
import spire.algebra.Eq
import spire.algebra.partial.{Groupoid, PartialAction}
import spire.syntax.eq._
import spire.syntax.partialAction._
import spire.laws._
import spire.std.boolean._
import com.faacets.operation.syntax.extractor._

object OperationLaws {

  def apply[A : Eq : Arbitrary, O : Eq : Arbitrary] = new OperationLaws[A, O] {
    def EquA = Eq[A]
    def EquO = Eq[O]
    def ArbA = implicitly[Arbitrary[A]]
    def ArbO = implicitly[Arbitrary[O]]
  }

}

trait OperationLaws[A, O] extends Laws {

  implicit def EquA: Eq[A]
  implicit def EquO: Eq[O]
  implicit def ArbA: Arbitrary[A]
  implicit def ArbO: Arbitrary[O]

  def groupoid(implicit
               O: Groupoid[O],
               P: PartialAction[A, O],
               E: OperationExtractor[A, O],
               C: Arbitrary[Canonical[A]],
               G: Operations.Generator[A, O]) = new OperationProperties(
    name = "operation",

    parent = None,

    bases = Seq("groupoid" -> PartialGroupLaws[O].groupoid),

    "extraction after transformation of canonical" -> forAll { (canonical: Canonical[A]) =>
      val Canonical(a) = canonical
      forAll(G.gen(a)) { o =>
        val transformed = (a <|+|? o).get
        val operationBack = transformed.forceExtract[O].operation
        val back = (transformed <|+|? operationBack).get
        a === back
      }
    },

    "cannot extract twice" -> forAll { (canonical: Canonical[A]) =>
      val Canonical(a) = canonical
      forAll(G.gen(a)) { o =>
        val transformed = (a <|+|? o).get
        val operationBack = transformed.forceExtract[O].extracted
        operationBack.canExtract[O] === false
      }
    }
  )

  class OperationProperties(
    val name: String,
    val parent: Option[OperationProperties],
    val bases: Seq[(String, Laws#RuleSet)],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent

}
