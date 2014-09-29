package psp
package std

import api._, Cmp._

object Order {
  def apply[A](f: (A, A) => Cmp): Order[A]  = new Impl[A](f)
  def order[A: Order] : Order[A]            = ?[Order[A]]
  def fold(xs: Cmp*): Cmp                   = xs.toSeq findOr (_ != EQ, EQ)
  def difference(diff: Long): Cmp           = if (diff < 0) LT else if (diff > 0) GT else EQ
  def create[A](ord: Ordering[A]): Order[A] = apply[A]((x, y) => difference(ord.compare(x, y)))

  final class OrderingImpl[A](cmp: (A, A) => Cmp) extends Ordering[A] {
    def compare(x: A, y: A): Int = cmp(x, y).intValue
  }
  final class Impl[-A](val f: (A, A) => Cmp) extends AnyVal with Order[A] {
    def compare(x: A, y: A): Cmp               = f(x, y)
    def toScalaOrdering[A1 <: A]: Ordering[A1] = new OrderingImpl[A1](compare)
  }
}
