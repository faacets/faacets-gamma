package com.faacets.operation.product

import net.alasc.partitions.{Partition, PartitionMap}
import spire.math.SafeLong

import scala.collection.immutable.BitSet

object PartitionHelpers {

  /*
    def bind2sub(N: Seq[SafeLong], i: SafeLong): Seq[SafeLong] =
      N.scanLeft((SafeLong(0), i)) { case ((rem, quot), n) => (quot % n, quot / n) }.map(_._1).tail

    def bsub2ind(N: Seq[SafeLong], I: Seq[SafeLong]): SafeLong =
      (N zip I).foldLeft((SafeLong(0), SafeLong(1))) { case ((tot, mul), (n, i)) => (tot + mul * i, mul * n) }._1

    def ind2sub(N: Seq[Int], i: Int): Seq[Int] =
      N.scanLeft((0, i)) { case ((rem, quot), n) => (quot % n, quot / n) }.map(_._1).tail

    def sub2ind(N: Seq[Int], I: Seq[Int]): Int =
      (N zip I).foldLeft((0, 1)) { case ((tot, mul), (n, i)) => (tot + mul * i, mul * n) }._1
*/


/*
    def nonTrivialBipartitions(n: Int): IndexedSeq[Vector[Boolean]] = {
      val bipartitions = (1 until (1 << n) - 1).map(integerToVector(_, n))
      val (singleA, restA) = bipartitions.partition(_.count(_ == true) == 1)
      val (singleB, rest) = restA.partition(_.count(_ == false) == 1)
      singleA ++ (singleB.map(_.map(!_))) ++ rest
    }

    // TODO: remove this helper?
    def allBipartitions(n: Int): IndexedSeq[Partition] = new IndexedSeq[Partition] {
      val bitset = scala.collection.immutable.BitSet(0 until n: _*)
      def length = ((1 << n) - 2)/2 // 2^n possibilities - 2 (we remove the two cases with an empty block)
      def apply(index: Int) = {
        val bits = index + 1 // the integer 0 is a bit vector representing a partition with an empty block
        val (block0, block1) = bitset.partition(b => (bits & (1 << b)) == 0)
        Partition(block0, block1)
      }
*/

}
