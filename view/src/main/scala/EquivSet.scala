package psp
package core

import psp.std._
import scala.collection.{ mutable, immutable }

abstract class Equiv[A] {
  def equiv(x: A, y: A): Boolean
  def hash(x: A): Int

  final class Wrap(val element: A) {
    def unwrap = element
    final override def equals(that: Any): Boolean = that match {
      case x: Wrap => try equiv(element, x.element.castTo[A]) catch { case _: ClassCastException => false }
      case _       => false
    }
    override def hashCode = hash(element)
    override def toString = s"$element (wrapped)"
  }
  def wrap(elem: A): Wrap = new Wrap(elem)
  def wrappedSet(xs: Foreach[A]): Set[Wrap] = (xs map wrap).toScalaSet
}

object Equiv {
  def universal[A]               = apply[A](_ == _, _.##)
  def reference[A <: AnyRef]     = apply[A](_ eq _, System.identityHashCode)
  def string[A](to: A => String) = apply[A]((x, y) => to(x) == to(y), x => to(x).##)

  def apply[A](cmp: (A, A) => Boolean, hashFn: A => Int): Equiv[A] = new Equiv[A] {
    def equiv(x: A, y: A) = cmp(x, y)
    def hash(x: A)        = hashFn(x)
  }
}

object EquivSet {
  def universal[A](xs: Foreach[A]): EquivSet[A]                     = apply[A](xs)(Equiv.universal[A])
  def reference[A <: AnyRef](xs: Foreach[A]): EquivSet[A]           = apply[A](xs)(Equiv.reference[A])
  def string[A](xs: Foreach[A])(to: A => String): EquivSet[A]       = apply[A](xs)(Equiv.string[A](to))
  def apply[A](xs: Foreach[A])(implicit cmp: Equiv[A]): EquivSet[A] = new EquivSet[A](xs, cmp)
}

final class EquivSet[A](basis: Foreach[A], equiv: Equiv[A]) extends immutable.Set[A] {
  import Equiv._
  private[this] val wrapSet = equiv wrappedSet basis

  def byReference(implicit ev: A <:< A with AnyRef)           = EquivSet.reference[A with AnyRef](basis map (x => ev(x)))
  def byUniversal                                             = by(universal[A])
  def byString                                                = by(string[A](s => s"$s"))
  def by[A1 >: A](equiv: Equiv[A1]): EquivSet[A1] = EquivSet[A1](basis)(equiv) // new EquivSet[A1](basis)(equiv)
  def grouped = (basis map equiv.wrap).toList groupBy (x => x) map { case (k, vs) => (k.unwrap, vs map (_.unwrap)) }

  override def size              = wrapSet.size
  def iterator: Iterator[A]      = wrapSet.iterator map (_.element)
  def contains(elem: A): Boolean = wrapSet(equiv wrap elem)
  def -(elem: A)                 = this
  def +(elem: A)                 = this
}
