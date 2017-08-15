package com.faacets
package laws

import spire.syntax.action._
import net.alasc.laws._
import net.alasc.perms.Perm
import net.alasc.std.seq._

import org.scalacheck._

import com.faacets.core._
import com.faacets.data.syntax.textable._
import com.faacets.laws.Operations.Generator
import com.faacets.operation.Reordering

object Reorderings {

  import net.alasc.laws.Permutations

  def genPerm(size: Int): Gen[Perm] = Permutations.permForSize(size)

  def genReordering(sourceScenario: Scenario): Gen[Reordering] = for {
    inputPerms <- Gen.sequence[Seq[Perm], Perm](sourceScenario.parties.map(party => genPerm(party.inputs.size)))
    partyPerm <- genPerm(sourceScenario.parties.size)
    partiesReorderedInputs = (sourceScenario.parties zip inputPerms).map {
      case (party, perm) => Party(party.inputs <|+| perm)
    }
    reorderedParties = partiesReorderedInputs <|+| partyPerm
    targetScenario = Scenario(reorderedParties)
  } yield Reordering(sourceScenario, targetScenario)

  implicit def arbReordering(implicit arbScenario: Arbitrary[Scenario]): Arbitrary[Reordering] =
    Arbitrary(arbScenario.arbitrary.flatMap(genReordering))

  implicit val reorderingGenerator: Generator[Expr, Reordering] =
    Generator((expr: Expr) => genReordering(expr.scenario) )

  implicit val reorderingInstances: Instances[Reordering] =
    Instances(Seq(
      Reordering("[(4 3 2)]".parseUnsafe[Scenario], "[(3 4 2)]".parseUnsafe[Scenario]),
      Reordering("[(4 3 2)]".parseUnsafe[Scenario], "[(4 3 2)]".parseUnsafe[Scenario]),
      Reordering("[(3 3 2)]".parseUnsafe[Scenario], "[(3 2 3)]".parseUnsafe[Scenario])
    ))

  implicit val reorderingCloner: Cloner[Reordering] =
    Cloner((r: Reordering) => Reordering(Scenarios.scenarioCloner.make(r.source),
      Scenarios.scenarioCloner.make(r.target)))

}
