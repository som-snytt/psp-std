package psp
package std

import PartialOrder._
import ThreeValue._

trait PartialOrder[A] extends Any {
  def partialCompare(lhs: A, rhs: A): Cmp
}
trait PartiallyOrdered[A] extends Any {
  def partialCompare(rhs: A): Cmp
}

object PartialOrder {
  type < = LT.type
  type ≤ = LE.type
  type ~ = NA.type
  type ≥ = GE.type
  type > = GT.type
  type ⊥ = NA.type

  sealed trait Cmp { def invert: Cmp }

  case object LT extends Cmp { def invert = GE }
  case object LE extends Cmp { def invert = GT }
  case object EQ extends Cmp { def invert = EQ }
  case object GE extends Cmp { def invert = LT }
  case object GT extends Cmp { def invert = LE }
  case object NA extends Cmp { def invert = NA }

  // case object LT extends Cmp { def invert = GE }
  // case object LE extends Cmp { def invert = GT }
  // case object EQ extends Cmp { def invert = EQ }
  // case object GE extends Cmp { def invert = LT }
  // case object GT extends Cmp { def invert = LE }
  // case object NA extends Cmp { def invert = NA }

  implicit class PartialOrderOps[A: PartialOrder](lhs: A) {
    private def pord = implicitly[PartialOrder[A]]
    // private def pcmp(rhs: A) =
    // def <(rhs: A): ThreeValue = pord.partialCompare(lhs, rhs) match {
    //   case LT      => True
    //   case NA | LE => Undefined
    //   case _       => False
    // }
    def <=(rhs: A): ThreeValue = pord.partialCompare(lhs, rhs) match {
      case LT | EQ | LE => True
      case NA           => Undefined
      case _            => False
    }
    def >(rhs: A): ThreeValue = !(this <= rhs)

    // pord.partialCompare(lhs, rhs) match {
    //   case GT      => True
    //   case NA | GE => Undefined
    //   case _       => False
    // }
    // def >=(rhs: A): ThreeValue = pord.partialCompare(lhs, rhs) match {
    //   case GT | EQ  | GE => True
    //   case NA            => Undefined
    //   case _             => False
    // }
    // def <==> (rhs: A): ThreeValue = pord.partialCompare(lhs, rhs) match {
    //   case EQ           => True
    //   case NA | LE | GE => Undefined
    //   case _            => False
    // }
  }
}

object PartiallyOrdered {
  final class Ops[A](val lhs: PartiallyOrdered[A]) extends AnyVal {
    def >=(rhs: A): ThreeValue = !(this < rhs)

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
    def >(rhs: A): ThreeValue = !(this <= rhs)

    // lhs.partialCompare(rhs) match {
    //   case GT      => True
    //   case NA | GE => Undefined
    //   case _       => False
    // }
    // def >=(rhs: A): ThreeValue = lhs.partialCompare(rhs) match {
    //   case GT | EQ  | GE => True
    //   case NA            => Undefined
    //   case _             => False
    // }
    def <==> (rhs: A): ThreeValue = lhs.partialCompare(rhs) match {
      case EQ           => True
      case NA | LE | GE => Undefined
      case _            => False
    }
  }
}
