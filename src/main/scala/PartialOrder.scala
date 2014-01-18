package psp
package core

import PartialOrder._
import ThreeValue._

trait PartialOrder[-A] extends Any {
  def partialCompare(lhs: A, rhs: A): Cmp
}

trait PartiallyOrdered[-A] extends Any {
  def partialCompare(that: A): Cmp
}

final class PartiallyOrderedOps[A](val lhs: PartiallyOrdered[A]) extends AnyVal {
  def <(rhs: A): ThreeValue = lhs.partialCompare(rhs) match {
    case LT      => True
    case NA | LE => Undefined
    case _       => False
  }
  def <=(rhs: A): ThreeValue = lhs.partialCompare(rhs) match {
    case LT | EQ | LE => True
    case NA           => Undefined
    case _            => False
  }
  def >(rhs: A): ThreeValue = lhs.partialCompare(rhs) match {
    case GT      => True
    case NA | GE => Undefined
    case _       => False
  }
  def >=(rhs: A): ThreeValue = lhs.partialCompare(rhs) match {
    case GT | EQ  | GE => True
    case NA            => Undefined
    case _             => False
  }
  def <==> (rhs: A): ThreeValue = lhs.partialCompare(rhs) match {
    case EQ           => True
    case NA | LE | GE => Undefined
    case _            => False
  }
}

object PartialOrder {
  sealed trait Cmp { def invert: Cmp }
  case object LT extends Cmp { def invert = GE }
  case object LE extends Cmp { def invert = GT }
  case object EQ extends Cmp { def invert = EQ }
  case object GE extends Cmp { def invert = LT }
  case object GT extends Cmp { def invert = LE }
  case object NA extends Cmp { def invert = NA }
}
