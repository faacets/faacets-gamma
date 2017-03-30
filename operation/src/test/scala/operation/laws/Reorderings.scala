package com.faacets
package operation
package laws
/*
import scala.annotation.tailrec

import org.scalacheck._

import spire.math.Rational
import spire.syntax.action._
import spire.syntax.cfor._

import net.alasc.math.{Domain, Perm}
import net.alasc.laws._
import net.alasc.std.seq._

import com.faacets.core._
import com.faacets.core.perm._
import core.laws.OperationGenerator
import core.laws.Scenarios.scenarioCloner

import data._

object Reorderings {

  import net.alasc.laws.Permutations

  def genPerm(size: Int): Gen[Perm] = Permutations.forSize[Perm](size)

  def genReordering(sourceScenario: Scenario): Gen[Reordering] = for {
    inputPerms <- Gen.sequence[Seq, Perm](sourceScenario.parties.map(party => genPerm(party.inputs.size)))
    partyPerm <- genPerm(sourceScenario.parties.size)
    partiesReorderedInputs = (sourceScenario.parties zip inputPerms).map {
      case (party, perm) => Party(party.inputs <|+| perm)
    }
    reorderedParties = partiesReorderedInputs <|+| partyPerm
    targetScenario = Scenario(reorderedParties)
  } yield Reordering(sourceScenario, targetScenario)

  implicit def arbReordering(implicit arbScenario: Arbitrary[Scenario]): Arbitrary[Reordering] =
    Arbitrary(arbScenario.arbitrary.flatMap(genReordering))

  implicit val reorderingGenerator: OperationGenerator[Expr, Reordering] =
    OperationGenerator( (expr: Expr) => genReordering(expr.scenario) )

  implicit val reorderingInstances: Instances[Reordering] =
    Instances(Seq(
      Reordering("[(4 3 2)]".fromText[Scenario], "[(3 4 2)]".fromText[Scenario]),
      Reordering("[(4 3 2)]".fromText[Scenario], "[(4 3 2)]".fromText[Scenario]),
      Reordering("[(3 3 2)]".fromText[Scenario], "[(3 2 3)]".fromText[Scenario])
    ))

  implicit val reorderingCloner: Cloner[Reordering] =
    Cloner((r: Reordering) => Reordering(scenarioCloner.make(r.source),
      scenarioCloner.make(r.target)))
}
*/