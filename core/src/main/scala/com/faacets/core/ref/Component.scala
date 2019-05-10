package com.faacets.core
package ref

import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.action._
import net.alasc.perms.{Cycles, Perm}
import spire.algebra.{Action, Eq, Group}
import cats.instances.list._
import scala.annotation.tailrec

/** Reference description of a [[Relabeling]] as product of party, input and output components. */
case class RefRelabeling(elements: List[Component]) { lhs =>
  import RefRelabeling.{rewrite, simplify, orderIncreasing, orderDecreasing, commutation}
  /** Returns a canonical form of that relabeling, where components are sorted and presented
    * in the following order:
    *
    * First, output permutations, sorted by party and then input. THen input permutations,
    * sorted by party. Finally, an optional party permutation.
    *
    * Sorting of party/input indices is done in increasing order.
    * */
  def canonicalIncreasing: RefRelabeling = RefRelabeling(rewrite(elements, simplify orElse orderIncreasing orElse commutation))

  // Same but with party/input indices in decreasing order
  def canonicalDecreasing: RefRelabeling = RefRelabeling(rewrite(elements, simplify orElse orderDecreasing orElse commutation))

}

final class RefRelabelingAlgebra extends Group[RefRelabeling] with Eq[RefRelabeling] {
  def eqv(x: RefRelabeling, y: RefRelabeling): Boolean = x.canonicalIncreasing.elements === y.canonicalIncreasing.elements
  def inverse(x: RefRelabeling): RefRelabeling = RefRelabeling(x.elements.reverse.map(_.inverse))
  def empty: RefRelabeling = RefRelabeling.id
  def combine(x: RefRelabeling, y: RefRelabeling): RefRelabeling = RefRelabeling(x.elements ++ y.elements)
}

final class POVMRefRelabelingAction extends Action[POVM, RefRelabeling] {
  def actl(x: RefRelabeling, povm: POVM): POVM = actr(povm, x.inverse)
  def actr(povm: POVM, x: RefRelabeling): POVM = x.elements.foldLeft(povm) { case (p, c) => c.action(p) }
}

object RefRelabeling {

  val id: RefRelabeling = RefRelabeling(Nil)
  private[this] val instance: RefRelabelingAlgebra = new RefRelabelingAlgebra
  implicit val group: Group[RefRelabeling] = instance
  implicit val equ: Eq[RefRelabeling] = instance
  implicit val povmAction: Action[POVM, RefRelabeling] = new POVMRefRelabelingAction

  /** A rewriting partial function that takes a list, and returns a result only when a rewriting applies. In that case,
    * it returns a pair of lists: the first list corresponds to the sublist that has been rewritten,
    * while the second list corresponds to the tail sublist that has not been modified.
    */
  type HeadRewrite[A] = PartialFunction[List[A], (List[A], List[A])]

  def rewrite[A](list: List[A], f: HeadRewrite[A]): List[A] = {
    val lifted: List[A] => Option[(List[A], List[A])] = f.lift

    /** Rewrites a list, which has been split in two parts.
      * The left part is not yet in canonical form, and is stored reversed in a list `reversedLeft`.
      * The right part is already in canonical form, and is stored as a list.
      *
      * Returns the canonical rewritten list.
      */
    @tailrec def iterate(reversedLeft: List[A], canonicalRight: List[A]): List[A] =
      reversedLeft match {
        case hd :: tl => lifted(hd :: canonicalRight) match {
          case Some((rewritten, untouched)) => iterate(rewritten.reverse ++ tl, untouched)
          case None => iterate(tl, hd :: canonicalRight)
        }
        case Nil => canonicalRight
      }

    iterate(list.reverse, Nil)
  }

  /** Group components that act on the same object, and removes identity components. */
  val simplify: HeadRewrite[Component] = {
    // Remove identity components
    case OutputComponent(p, x, a) :: tl if a.isId => (Nil, tl)
    case InputComponent(p, x) :: tl if x.isId => (Nil, tl)
    case PartyComponent(p) :: tl if p.isId => (Nil, tl)

    case OutputComponent(p1, x1, a1) :: OutputComponent(p2, x2, a2) :: tl if p1 == p2 && x1 == x2 =>
      (OutputComponent(p1, x1, a1 |+| a2) :: Nil, tl)
    case InputComponent(p1, x1) :: InputComponent(p2, x2) :: tl if p1 == p2 =>
      (InputComponent(p1, x1 |+| x2) :: Nil, tl)
    case (PartyComponent(p1)) :: (PartyComponent(p2)) :: tl =>
      (PartyComponent(p1 |+| p2) :: Nil, tl)
  }

  /** Sorts commuting components of the same type, indices increasing. */
  val orderIncreasing: HeadRewrite[Component] = {
    case (c1@OutputComponent(p1, x1, a1)) :: (c2@OutputComponent(p2, x2, a2)) :: tl if p1 > p2 || (p1 == p2 && x1 > x2) =>
      (c2 :: c1 :: Nil, tl)
    case (c1@InputComponent(p1, x1)) :: (c2@InputComponent(p2, x2)) :: tl if p1 > p2 =>
      (c2 :: c1 :: Nil, tl)
  }

  /** Sorts commuting components of the same type, indices decreasing. */
  val orderDecreasing: HeadRewrite[Component] = {
    case (c1@OutputComponent(p1, x1, a1)) :: (c2@OutputComponent(p2, x2, a2)) :: tl if p1 < p2 || (p1 == p2 && x1 < x2) =>
      (c2 :: c1 :: Nil, tl)
    case (c1@InputComponent(p1, x1)) :: (c2@InputComponent(p2, x2)) :: tl if p1 < p2 =>
      (c2 :: c1 :: Nil, tl)
  }

  /** Applies commutation between component types. */
  val commutation: HeadRewrite[Component] = {
    case (c1@InputComponent(p1, x1)) :: OutputComponent(p2, x2, a2) :: tl if p1 == p2 =>
      (OutputComponent(p1, x1.invImage(x2), a2) :: c1 :: Nil, tl)
    case (c1@InputComponent(p1, x1)) :: (c2@OutputComponent(p2, x2, a2)) :: tl => // p1 != p2
      (c2 :: c1 :: Nil, tl)
    case (c1@PartyComponent(p1)) :: OutputComponent(p2, x2, a2) :: tl =>
      (OutputComponent(p1.invImage(p2), x2, a2) :: c1 :: Nil, tl)
    // Commute input and party
    case (c1@PartyComponent(p1)) :: InputComponent(p2, x2) :: tl =>
      (InputComponent(p1.invImage(p2), x2) :: c1 :: Nil, tl)
    case (c1: InputComponent) :: (c2: PartyComponent) :: tl =>
      (c2 :: c1 :: Nil, tl)
  }

  def apply(elements: List[Component]): RefRelabeling = new RefRelabeling(elements)

}

/** A component of a relabeling, which represents a permutation of a single label (party, input or output). */
abstract class Component {
  def toRelabeling: Relabeling
  def action(povm: POVM): POVM
  def inverse: Component
}

object Component {
  implicit val equ: Eq[Component] = Eq.fromUniversalEquals
}

case class OutputComponent(p: Int, x: Int, a: Perm) extends Component {
  override def toString: String = Party.prefixes(p) + x.toString + Cycles.fromPerm(a).string
  def toRelabeling: Relabeling = Relabeling(Map(p -> PartyRelabeling(Map(x -> a), Perm.id)), Perm.id)
  def action(povm: POVM): POVM = povm match {
    case POVM(p1, x1, a1) if p == p1 && x == x1 => POVM(p1, x1, a1 <|+| a)
    case _ => povm
  }
  def inverse: OutputComponent = OutputComponent(p, x, a.inverse)
}

case class InputComponent(p: Int, x: Perm) extends Component {
  override def toString: String = Party.prefixes(p) + Cycles.fromPerm(x).string
  def toRelabeling: Relabeling = Relabeling(Map(p -> PartyRelabeling(Map.empty[Int, Perm], x)), Perm.id)
  def action(povm: POVM): POVM = povm match {
    case POVM(p1, x1, a1) if p == p1 => POVM(p1, x1 <|+| x, a1)
    case _ => povm
  }
  def inverse: InputComponent = InputComponent(p, x.inverse)
}

case class PartyComponent(p: Perm) extends Component {
  override def toString: String = Cycles.fromPerm(p).stringUsing(Party.prefixes(_))
  def toRelabeling: Relabeling = Relabeling(Map.empty[Int, PartyRelabeling], p)
  def action(povm: POVM): POVM = povm match {
    case POVM(p1, x1, a1) => POVM(p1 <|+| p, x1, a1)
  }
  def inverse: PartyComponent = PartyComponent(p.inverse)
}
