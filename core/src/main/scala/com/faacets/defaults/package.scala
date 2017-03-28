package com.faacets

import scala.util.Random

import spire.util.Opt

package object defaults
  extends net.alasc.perms.Algorithms(Opt(Random), false, true)
    with spire.syntax.AllSyntax
    with spire.std.AnyInstances
    with net.alasc.syntax.AllSyntax
    with net.alasc.std.AnyInstances
    with scalin.syntax.AllSyntax
    with com.faacets.consolidate.syntax.AllSyntax
    with com.faacets.consolidate.instances.AllInstances
    with com.faacets.core.syntax.AllSyntax
    with com.faacets.data.instances.AllInstances
    with com.faacets.data.syntax.AllSyntax {

  implicit def vecEngine[A]: scalin.VecEngine[A, scalin.immutable.DenseVec[A]] = scalin.immutable.DenseVec.defaultEngine[A]

  implicit def matEngine[A]: scalin.MatEngine[A, scalin.immutable.DenseMat[A]] = scalin.immutable.DenseMat.defaultEngine[A]

  type Vec[A] = scalin.immutable.Vec[A]
  val Vec = scalin.immutable.Vec

  type Mat[A] = scalin.immutable.Mat[A]
  val Mat = scalin.immutable.Mat

  type Rational = spire.math.Rational
  val Rational = spire.math.Rational

  type SafeLong = spire.math.SafeLong
  val SafeLong = spire.math.SafeLong

  type Grp[G] = net.alasc.finite.Grp[G]
  val Grp = net.alasc.finite.Grp

  type Perm = net.alasc.perms.Perm
  val Perm = net.alasc.perms.Perm

}
