package psp
package std

trait PartialOrder[-A] extends Any {
  def partialCompare(lhs: A, rhs: A): PartialOrder.PCmp
}

object PartialOrder {
  def apply[A](f: (A, A) => PCmp): PartialOrderClass[A] = new PartialOrderClass[A](f)

  implicit def orderIsPartialOrder[A: Order] : PartialOrder[A] = apply[A]((x, y) => translate(x compare y))

  def translate(cmp: Cmp): PCmp = cmp match {
    case api.Cmp.LT => LT
    case api.Cmp.GT => GT
    case _          => EQ
  }

  sealed trait PCmp { def invert: PCmp }
  case object LT extends PCmp { def invert = GE }
  case object LE extends PCmp { def invert = GT }
  case object EQ extends PCmp { def invert = EQ }
  case object GE extends PCmp { def invert = LT }
  case object GT extends PCmp { def invert = LE }
  case object NA extends PCmp { def invert = NA }

  final class PartialOrderClass[A](private val f: (A, A) => PCmp) extends AnyVal with PartialOrder[A] {
    def partialCompare(x: A, y: A): PCmp = f(x, y)
  }
}
