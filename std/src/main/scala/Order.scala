package psp
package std

import Order._, Cmp._

/** Contravariance vs. implicits, the endless battle.
 *  We return a three value enum from compare in preference to
 *  a wild stab into the 2^32 states of an Int. This is a
 *  controversial thing to do, in the year 2014. Not joking.
 *  (I was joking about the enum though, since scala has no
 *  usable enum. We flounder along with a value class instead.)
 */
trait Order[-A] extends Any {
  def compare(x: A, y: A): Cmp
}

object Order {
  /** Used when you have an Order[B] but want an Order[A],
   *  e.g. Order.by[String](_.length) produces an Order[String].
   *  The class By has no purpose other than to assist type inference.
   */
  def by[A] : By[A] = new By[A]

  def apply[A](implicit ord: Order[A]): Order[A] = ord

  implicit val IntOrder  = new OrderClass[Int]((x, y) => Cmp(x - y))
  implicit val LongOrder = new OrderClass[Long]((x, y) => Cmp(x - y))

  final class Cmp private (val intValue: Int) extends AnyVal {
    override def toString = this match {
      case LT => "LT"
      case EQ => "EQ"
      case GT => "GT"
    }
  }
  object Cmp {
    def apply(diff: Long): Cmp = if (diff < 0) LT else if (diff > 0) GT else EQ

    final val LT = new Cmp(-1)
    final val EQ = new Cmp(0)
    final val GT = new Cmp(1)
  }
  final class By[A] {
    def apply[B](f: A => B)(implicit ord: Order[B]): Order[A] = ord on f
  }
  final class OrderClass[A](f: (A, A) => Cmp) extends Order[A] {
    def compare(x: A, y: A): Cmp = f(x, y)
  }
  final class Ops[A](val lhs: A)(implicit ord: Order[A]) {
    def compare(rhs: A)     = ord.compare(lhs, rhs)
    def <(rhs: A): Boolean  = compare(rhs) == LT
    def <=(rhs: A): Boolean = compare(rhs) != GT
    def >=(rhs: A): Boolean = compare(rhs) != LT
    def >(rhs: A): Boolean  = compare(rhs) == GT
    def min(rhs: A): A      = if (compare(rhs) == LT) lhs else rhs
    def max(rhs: A): A      = if (compare(rhs) == GT) lhs else rhs
  }
  final class OrderOps[A](val ord: Order[A]) extends AnyVal {
    def on[B](f: B => A): Order[B] = new OrderClass[B]((x, y) => ord.compare(f(x), f(y)))
  }
}
