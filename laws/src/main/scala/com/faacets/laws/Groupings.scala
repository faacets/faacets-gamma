package com.faacets
package laws

import scala.annotation.tailrec
import org.scalacheck._
import spire.math.Rational
import spire.syntax.action._
import net.alasc.laws._
import net.alasc.std.seq._
import core._
import core.perm._
import operation._
import lifting._
import data._
import net.alasc.domains.Partition
import data.syntax.textable._

object Groupings {

  def genPartitionOfNumBlocks(n: Int): Gen[Partition] = for {
    nOfEach <- Gen.containerOfN[Seq, Int](n, Gen.oneOf(1, 1, 1, 2, 3))
    seq = (0 until n).flatMap( i => Seq.fill(nOfEach(i))(i) )
    perm <- Permutations.permForSize(seq.size)
  } yield Partition.fromSeq((seq: Seq[Int]) <|+| perm)

  def genInputGrouping(nA: Int): Gen[InputGrouping] =
    genPartitionOfNumBlocks(nA).map( InputGrouping(_) )

  def genPartyGrouping(party: Party): Gen[PartyGrouping] =
    Gen.sequence[Seq[InputGrouping], InputGrouping](party.inputs.map(genInputGrouping(_)))
      .map(PartyGrouping(_))

  def genGrouping(scenario: Scenario): Gen[Grouping] =
    Gen.sequence[Seq[PartyGrouping], PartyGrouping](scenario.parties.map(genPartyGrouping(_)))
      .map(Grouping(_))

  implicit def arbGrouping(implicit arbScenario: Arbitrary[Scenario]): Arbitrary[Grouping] =
    Arbitrary(arbScenario.arbitrary.flatMap(genGrouping(_)))

  implicit val groupingInstances: Instances[Grouping] =
    Instances(Seq("[({0 0 1} 2)]", "[(2 2)]").map(_.parseUnsafe[Grouping]))

  implicit val inputGroupingCloner: Cloner[InputGrouping] =
    Cloner((ig: InputGrouping) => InputGrouping(Partitions.partitionCloner.make(ig.partition)))

  implicit val partyGroupingCloner: Cloner[PartyGrouping] =
    Cloner((pg: PartyGrouping) => PartyGrouping(pg.inputs.map(inputGroupingCloner.make)))

  implicit val groupingCloner: Cloner[Grouping] =
    Cloner((g: Grouping) => Grouping(g.parties.map(partyGroupingCloner.make)))

}
