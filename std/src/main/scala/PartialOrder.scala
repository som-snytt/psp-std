package psp
package std

import PartialOrder.Cmp

trait PartialOrder[-A] extends Any {
  def partialCompare(lhs: A, rhs: A): Cmp
}

object PartialOrder {
  def apply[A](f: (A, A) => Cmp): PartialOrderClass[A] = new PartialOrderClass[A](f)

  implicit def orderIsPartialOrder[A: Order] : PartialOrder[A] = apply[A]((x, y) => translate(x compare y))

  def translate(cmp: Order.Cmp): PartialOrder.Cmp = cmp match {
    case Order.Cmp.LT => LT
    case Order.Cmp.GT => GT
    case _            => EQ
  }

  sealed trait Cmp { def invert: Cmp }
  case object LT extends Cmp { def invert = GE }
  case object LE extends Cmp { def invert = GT }
  case object EQ extends Cmp { def invert = EQ }
  case object GE extends Cmp { def invert = LT }
  case object GT extends Cmp { def invert = LE }
  case object NA extends Cmp { def invert = NA }

  final class PartialOrderClass[A](private val f: (A, A) => Cmp) extends AnyVal with PartialOrder[A] {
    def partialCompare(x: A, y: A): Cmp = f(x, y)
  }
}
