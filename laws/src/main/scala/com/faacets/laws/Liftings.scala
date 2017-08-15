package com.faacets
package laws

import net.alasc.laws.{Cloner, Instances}
import net.alasc.partitions.Partition

import org.scalacheck.{Arbitrary, Gen}

import com.faacets.core.{Expr, Scenario}
import com.faacets.data.syntax.textable._
import com.faacets.operation.Lifting
import com.faacets.operation.lifting.{Grouping, InputGrouping, PartyGrouping}

object Liftings {

  import Groupings._

  def removeLiftedInput(partyGrouping: PartyGrouping): Gen[PartyGrouping] =
    if (!partyGrouping.hasLiftedInputs) Gen.fail[PartyGrouping] else {
      val liftedInputs: Seq[Int] = partyGrouping.inputs.indices.filter(i => partyGrouping.inputs(i).isLiftedInput)
      for {
        x <- Gen.oneOf(liftedInputs)
      } yield PartyGrouping(partyGrouping.inputs.take(x) ++ partyGrouping.inputs.drop(x + 1))
    }

  def addLiftedInput(partyGrouping: PartyGrouping): Gen[PartyGrouping] =
    for {
      x <- Gen.oneOf(partyGrouping.inputs.indices)
      newSize <- Gen.choose(2, 4)
      newInput = InputGrouping(Partition.fromSeq(Seq.fill(newSize)(0)))
    } yield PartyGrouping((partyGrouping.inputs.take(x) :+ newInput) ++ partyGrouping.inputs.drop(x))

  def replaceOutput(partyGrouping: PartyGrouping): Gen[PartyGrouping] =
    for {
      x <- Gen.oneOf(partyGrouping.inputs.indices)
      newPartition <- genPartitionOfNumBlocks(partyGrouping.inputs(x).partition.nBlocks)
      newInput = InputGrouping(newPartition)
    } yield partyGrouping.updated(x, newInput)

  def mutatePartyGrouping(partyGrouping: PartyGrouping): Gen[PartyGrouping] =
    Gen.frequency(3 -> replaceOutput(partyGrouping), 2 -> removeLiftedInput(partyGrouping), 1 -> addLiftedInput(partyGrouping))

  def mutateGrouping(grouping: Grouping): Gen[Grouping] = for {
    p <- Gen.oneOf(grouping.parties.indices)
    newParty <- mutatePartyGrouping(grouping.parties(p))
  } yield grouping.updated(p, newParty)

  def mutateGroupingNTimes(n: Int, grouping: Grouping): Gen[Grouping] =
    if (n == 0) Gen.const(grouping) else mutateGroupingNTimes(n - 1, grouping)

  def genLifting(sourceGrouping: Grouping): Gen[Lifting] = mutateGroupingNTimes(5, sourceGrouping).map( Lifting(sourceGrouping, _) )

  def genLifting(scenario: Scenario): Gen[Lifting] = for {
    source <- genGrouping(scenario)
    target <- genGrouping(scenario)
  } yield Lifting(source, target)

  implicit def arbLifting(implicit arbScenario: Arbitrary[Scenario]): Arbitrary[Lifting] =
    Arbitrary(arbScenario.arbitrary.flatMap(genLifting(_)))

  implicit val liftingGenerator: Operations.Generator[Expr, Lifting] =
    Operations.Generator(expr => genLifting(Grouping(expr.scenario, expr.symmetryGroup)))

  implicit val liftingInstances: Instances[Lifting] =
    Instances(Seq(
      Lifting("[(2 2) (2 2)]".parseUnsafe[Grouping], "[({0 0 1} 2) (2 2)]".parseUnsafe[Grouping]),
      Lifting("[({0 0 1 2} 3)]".parseUnsafe[Grouping], "[(3 3)]".parseUnsafe[Grouping])
    ))

  implicit val liftingCloner: Cloner[Lifting] =
    Cloner((l: Lifting) => Lifting(groupingCloner.make(l.source), groupingCloner.make(l.target)))

}
