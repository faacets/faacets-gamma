package com.faacets.core
package ref

import spire.syntax.group._
import spire.syntax.action._
import net.alasc.perms.{Cycles, Perm}
import spire.algebra.{Action, Eq, Group}

import scala.annotation.tailrec

case class RefRelabeling(elements: List[Component]) { lhs =>
  def combine(rhs: RefRelabeling): RefRelabeling = RefRelabeling(lhs.elements ++ rhs.elements)
  def inverse: RefRelabeling = RefRelabeling(elements.reverse.map(_.inverse))
  def action(povm: POVM): POVM = elements.foldLeft(povm) { case (p, c) => c.action(p) }
}

final class RefRelabelingGroup extends Group[RefRelabeling] {
  def inverse(rr: RefRelabeling): RefRelabeling = rr.inverse
  def empty: RefRelabeling = RefRelabeling(Nil)
  def combine(x: RefRelabeling, y: RefRelabeling): RefRelabeling = x.combine(y)
}

final class POVMRefRelabelingAction extends Action[POVM, RefRelabeling] {
  def actl(rr: RefRelabeling, p: POVM): POVM = actr(p, rr.inverse)
  def actr(p: POVM, rr: RefRelabeling): POVM = rr.action(p)
}

object RefRelabeling {

  implicit val group: Group[RefRelabeling] = new RefRelabelingGroup
  implicit val equ: Eq[RefRelabeling] = Eq.fromUniversalEquals
  implicit val povmAction: Action[POVM, RefRelabeling] = new POVMRefRelabelingAction

  def forceCanonicalizeStep(elements: List[Component]): List[Component] =
    canonicalizeStep(elements).getOrElse(elements)

  def canonicalizeStep(elements: List[Component]): Option[List[Component]] = elements match {
    case Nil => None
      // Remove identity components
    case OutputComponent(p, x, a) :: tl if a.isId => Some(forceCanonicalizeStep(tl))
    case InputComponent(p, x) :: tl if x.isId => Some(forceCanonicalizeStep(tl))
    case PartyComponent(p) :: tl if p.isId => Some(forceCanonicalizeStep(tl))

      // Sort (and group) output components
    case (c1@OutputComponent(p1, x1, a1)) :: (c2@OutputComponent(p2, x2, a2)) :: tl if p1 > p2 || (p1 == p2 && x1 > x2) =>
      Some(c2 :: forceCanonicalizeStep(c1 :: tl))
    case OutputComponent(p1, x1, a1) :: OutputComponent(p2, x2, a2) :: tl if p1 == p2 && x1 == x2 =>
      Some(OutputComponent(p1, x1, a1 |+| a2) :: forceCanonicalizeStep(tl))
      // OutputComponent(p1, x1, a1) :: OutputComponent(p2, x2, a2) :: tl if (p1 == p2 && x1 < x2) || p1 < p2
      // already in order

      // Sort (and group) input components
    case (c1@InputComponent(p1, x1)) :: (c2@InputComponent(p2, x2)) :: tl if p1 > p2 =>
      Some(c2 :: forceCanonicalizeStep(c1 :: tl))
    case InputComponent(p1, x1) :: InputComponent(p2, x2) :: tl if p1 == p2 =>
      Some(InputComponent(p1, x1 |+| x2) :: forceCanonicalizeStep(tl))
      // InputComponent(p1, x1) :: InputComponent(p2, x2) :: tl if p1 < p2
      // in order
      // Group party components
    case (PartyComponent(p1)) :: (PartyComponent(p2)) :: tl =>
      Some(PartyComponent(p1 |+| p2) :: forceCanonicalizeStep(tl))
      // Commute output and input
    case (c1@InputComponent(p1, x1)) :: OutputComponent(p2, x2, a2) :: tl if p1 == p2 =>
      Some(OutputComponent(p1, x1.invImage(x2), a2) :: forceCanonicalizeStep(c1 :: tl))
    case (c1@InputComponent(p1, x1)) :: (c2@OutputComponent(p2, x2, a2)) :: tl => // p1 != p2
      Some(c2 :: forceCanonicalizeStep(c1 :: tl))
    case (c1: OutputComponent) :: (c2: InputComponent) :: tl =>
      canonicalizeStep(c2 :: tl).map(c1 :: _)
      // Commute output and party
    case (c1@PartyComponent(p1)) :: OutputComponent(p2, x2, a2) :: tl =>
      Some(OutputComponent(p1.invImage(p2), x2, a2) :: forceCanonicalizeStep(c1 :: tl))
    case (c1: OutputComponent) :: (c2: PartyComponent) :: tl =>
      canonicalizeStep(c2 :: tl).map(c1 :: _)
      // Commute input and party
    case (c1@PartyComponent(p1)) :: InputComponent(p2, x2) :: tl =>
      Some(InputComponent(p1.invImage(p2), x2) :: forceCanonicalizeStep(c1 :: tl))
    case (c1: InputComponent) :: (c2: PartyComponent) :: tl =>
      canonicalizeStep(c2 :: tl).map(c1 :: _)

      // is already in order
    case c1 :: tl =>
      canonicalizeStep(tl).map(tl1 => c1 :: tl1)
  }

  @tailrec
  def canonicalize(elements: List[Component]): List[Component] = canonicalizeStep(elements) match {
    case Some(newList) => canonicalize(newList) // if it has changed, perform another run
    case None => elements
  }

  def apply(elements: List[Component]): RefRelabeling = new RefRelabeling(canonicalize(elements))

}

abstract class Component {
  def toRelabeling: Relabeling
  def action(povm: POVM): POVM
  def inverse: Component
}

abstract class ComponentForParty extends Component {
  def p: Int
  def partyRelabeling: PartyRelabeling
  def toRelabeling = partyRelabeling.forParty(p)
}

case class OutputComponent(p: Int, x: Int, a: Perm) extends ComponentForParty {
  override def toString = Party.prefixes(p) + x.toString + Cycles.fromPerm(a).string
  def partyRelabeling = PartyRelabeling.OutputComponent(x, a).get
  def action(povm: POVM): POVM = povm match {
    case POVM(p1, x1, a1) if p == p1 && x == x1 => POVM(p1, x1, a1 <|+| a)
    case _ => povm
  }
  def inverse: OutputComponent = OutputComponent(p, x, a.inverse)
}

case class InputComponent(p: Int, x: Perm) extends ComponentForParty {
  override def toString = Party.prefixes(p) + Cycles.fromPerm(x).string
  def partyRelabeling = PartyRelabeling.InputComponent(x).get
  def action(povm: POVM): POVM = povm match {
    case POVM(p1, x1, a1) if p == p1 => POVM(p1, x1 <|+| x, a1)
    case _ => povm
  }
  def inverse: InputComponent = InputComponent(p, x.inverse)
}

case class PartyComponent(p: Perm) extends Component {
  override def toString = Cycles.fromPerm(p).stringUsing(Party.prefixes(_))
  def toRelabeling = Relabeling(Map.empty[Int, PartyRelabeling], p)
  def action(povm: POVM): POVM = povm match {
    case POVM(p1, x1, a1) => POVM(p1 <|+| p, x1, a1)
  }
  def inverse: PartyComponent = PartyComponent(p.inverse)
}
