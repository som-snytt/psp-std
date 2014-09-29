package psp
package std

import api._

object Eq {
  final class Impl[-A](val f: (A, A) => Boolean) extends AnyVal with Eq[A] { def equiv(x: A, y: A) = f(x, y) }
  def native[A](): Impl[A]                    = new Impl[A](_ == _)
  def apply[A](f: (A, A) => Boolean): Impl[A] = new Impl[A](f)
}
object HashEq {
  def apply[A](cmp: (A, A) => Boolean, hashFn: A => Int): Impl[A] = new Impl[A](cmp, hashFn)

  def universal[A] : Impl[A]           = apply[A](_ == _, _.##)
  def reference[A <: AnyRef] : Impl[A] = apply[A](_ eq _, System.identityHashCode)
  def shown[A: Show] : Impl[A]         = apply[A](_.to_s == _.to_s, _.to_s.##)
  def native[A](eqs: Eq[A]): Impl[A]   = apply[A](eqs.equiv, _.##)

  final case class Wrap[A: HashEq](value: A) {
    override def hashCode = value.hash
    override def equals(x: Any): Boolean = x match {
      case Wrap(that) => value === that.castTo[A]
      case _          => false
    }
    override def toString = pp"$value"
  }
  final class Impl[-A](cmp: (A, A) => Boolean, h: A => Int) extends HashEq[A] {
    def equiv(x: A, y: A) = cmp(x, y)
    def hash(x: A)        = h(x)
  }
}
object OrderEq {
  final class Impl[-A](val f: (A, A) => Cmp) extends AnyVal with OrderEq[A] {
    def equiv(x: A, y: A)   = f(x, y) == Cmp.EQ
    def compare(x: A, y: A) = f(x, y)
  }
  def apply[A](f: (A, A) => Cmp): OrderEq[A] = new Impl[A](f)
}
object PartialOrderEq {
  final class Impl[-A](val f: (A, A) => PCmp) extends AnyVal with PartialOrderEq[A] {
    def equiv(x: A, y: A)          = f(x, y) == PCmp.EQ
    def partialCompare(x: A, y: A) = f(x, y)
  }
  def apply[A](f: (A, A) => PCmp): PartialOrderEq[A] = new Impl[A](f)
}

object Order {
  import Cmp._

  def apply[A](f: (A, A) => Cmp): Impl[A]   = new Impl[A](f)
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

object Builds {
  def apply[Elem, To](f: Foreach[Elem] => To): Impl[Elem, To] = new Impl(f)
  def wrap[Elem, To](z: CanBuild[Elem, To]): Impl[Elem, To]   = apply[Elem, To](xs => z() ++= new Foreach.ToScala(xs) result)

  final class Impl[Elem, To](val f: Foreach[Elem] => To) extends AnyVal with Builds[Elem, To] {
    def build(xs: Foreach[Elem]): To = f(xs)
  }
}

final class Utf8(val bytes: Array[Byte]) extends AnyVal {
  def chars: Chars = scala.io.Codec fromUTF8 bytes
  def to_s: String = new String(chars)
  override def toString = to_s
}
