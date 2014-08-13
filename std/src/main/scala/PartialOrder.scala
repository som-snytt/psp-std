package psp
package std

import PartialOrder._
import ThreeValue._

trait PartialOrder[-A] extends Any {
  def partialCompare(lhs: A, rhs: A): Cmp
}

object PartialOrder {
  // type < = LT.type
  // type ≤ = LE.type
  // type ~ = NA.type
  // type ≥ = GE.type
  // type > = GT.type
  // type ⊥ = NA.type

  sealed trait Cmp { def invert: Cmp }
  case object LT extends Cmp { def invert = GE }
  case object LE extends Cmp { def invert = GT }
  case object EQ extends Cmp { def invert = EQ }
  case object GE extends Cmp { def invert = LT }
  case object GT extends Cmp { def invert = LE }
  case object NA extends Cmp { def invert = NA }

  final class Ops[A](val lhs: A)(implicit pord: PartialOrder[A]) {
    def partial: this.type = this
    def partialCompare(rhs: A): Cmp = pord.partialCompare(lhs, rhs)
    private def evaluate(rhs: A)(trues: Cmp*)(undefs: Cmp*): ThreeValue = {
      val res = partialCompare(rhs)
      if (trues contains res) True
      else if (undefs contains res) Undefined
      else False
    }
    def <(rhs: A): ThreeValue     = evaluate(rhs)(LT)(NA, LE)
    def <=(rhs: A): ThreeValue    = evaluate(rhs)(LT, LE, EQ)(NA)
    def >=(rhs: A): ThreeValue    = evaluate(rhs)(GT, GE, EQ)(NA)
    def >(rhs: A): ThreeValue     = evaluate(rhs)(GT)(NA, GE)
    def <==> (rhs: A): ThreeValue = evaluate(rhs)(EQ)(NA, LE, GE)
  }
}
