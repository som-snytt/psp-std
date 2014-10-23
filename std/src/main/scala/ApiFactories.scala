package psp
package std

import api._

object PartialOrder {
  final class Impl[-A](val f: (A, A) => PCmp) extends AnyVal with PartialOrder[A] {
    def partialCompare(x: A, y: A) = f(x, y)
  }
  def apply[A](f: (A, A) => PCmp): PartialOrder[A] = new Impl[A](f)
}

object Order {
  import Cmp._

  def apply[A](f: (A, A) => Cmp): Impl[A]    = new Impl[A](f)
  def natural[A <: Comparable[A]](): Impl[A] = fromInt[A](_ compareTo _)
  def order[A: Order] : Order[A]             = ?[Order[A]]
  def fold(xs: Cmp*): Cmp                    = xs.toSeq findOr (_ != EQ, EQ)
  def create[A](ord: Ordering[A]): Order[A]  = apply[A]((x, y) => newCmp(ord.compare(x, y)))

  def fromInt[A](f: (A, A) => Int): Impl[A]   = new Impl[A]((x, y) => newCmp(f(x, y)))
  def fromLong[A](f: (A, A) => Long): Impl[A] = new Impl[A]((x, y) => newCmp(f(x, y)))

  final class OrderingImpl[A](cmp: (A, A) => Cmp) extends Ordering[A] {
    def compare(x: A, y: A): Int = cmp(x, y).intValue
  }
  final class Impl[-A](val f: (A, A) => Cmp) extends AnyVal with Order[A] {
    def compare(x: A, y: A): Cmp                = f(x, y)
    def toScalaOrdering[A1 <: A] : Ordering[A1] = new OrderingImpl[A1](compare)
    def toComparator[A1 <: A] : Comparator[A1]  = JavaComparator[A1]((x, y) => f(x, y).intValue)
  }
}

object Builds {
  def apply[Elem, To](f: Foreach[Elem] => To): Builds[Elem, To] = new Impl(f)
  def wrap[Elem, To](z: CanBuild[Elem, To]): Builds[Elem, To]   = new Impl(xs => z() doto (b => xs foreach (b += _)) result)

  final class Impl[Elem, To](val f: Foreach[Elem] => To) extends AnyVal with Builds[Elem, To] {
    def build(xs: Foreach[Elem]): To   = f(xs)
    def apply(mf: Suspended[Elem]): To = build(Foreach(mf))
  }
}

object Zero {
  def apply[A](zero: A): Impl[A] = new Impl[A](zero)
  final class Impl[A](val zero: A) extends AnyVal with Zero[A] {
    def isZero(x: A): Boolean = zero == x
  }
}

object Sums {
  final class Impl[A](f: BinOp[A], z: Zero[A]) extends Sums[A] {
    def zero: A = z.zero
    def sum(x: A, y: A): A = f(x, y)
  }
  def apply[A](f: BinOp[A])(implicit z: Zero[A]): Sums[A] = new Impl(f, z)
}
