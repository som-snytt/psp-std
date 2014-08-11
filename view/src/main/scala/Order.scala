package psp
package core

trait Order[-A] {
  def compare(x: A, y: A): Order.Cmp
}

final class OrderImpl[-A](f: (A, A) => Order.Cmp) extends Order[A] {
  def compare(x: A, y: A): Order.Cmp = f(x, y)
}

object Order {
  def apply[A](implicit ord: Order[A]): Order[A] = ord

  implicit def scalaOrderingToOrder[A](implicit ord: Ordering[A]): Order[A] = new OrderImpl[A]((x, y) => cmp(ord.compare(x, y)))

  implicit final class OrderOps[A](ord: Order[A]) {
    def by[B](f: B => A): Order[B] = new OrderImpl[B]((x, y) => ord.compare(f(x), f(y)))
    def min(x: A, y: A): A = if (ord.compare(x, y) == LT) x else y
    def max(x: A, y: A): A = if (ord.compare(x, y) == GT) x else y
  }

  def cmp(x: Int): Cmp = if (x < 0) LT else if (x > 0) GT else EQ

  sealed trait Cmp { def intValue: Int }
  case object LT extends Cmp { def intValue = -1 }
  case object EQ extends Cmp { def intValue = 0 }
  case object GT extends Cmp { def intValue = 1 }
}
