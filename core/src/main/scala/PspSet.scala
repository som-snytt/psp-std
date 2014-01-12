package psp
package core

import scala.collection.{ mutable, immutable }

final class IntensionalSet[-A](contains: A => Boolean) extends (A => Boolean) {
  def apply(x: A): Boolean                   = contains(x)
  def map[B](f: B => A)                      = IntensionalSet[B](x => apply(f(x)))
  def withFilter[A1 <: A](p: A1 => Boolean)  = IntensionalSet[A1](x => apply(x) && p(x))
  def union[A1 <: A](xs: IntensionalSet[A1]) = IntensionalSet[A1](x => apply(x) || xs(x))

  override def toString = s"Intensional Set"
}
object IntensionalSet {
  def apply[A](f: A => Boolean) = new IntensionalSet[A](f)
}

final class Wrap[A, B](val value: A)(equivBy: A => B) {
  private[this] val lhs = equivBy(value)
  final override def equals(that: Any): Boolean = that match {
    case x: Wrap[_, _] => lhs == equivBy(x.value.castTo[A])
    case _             => false
  }
  override def hashCode = lhs.##
  override def toString = s"$value (wrapped as: $lhs)"
}

class CustomEqualitySet[A, B](basis: Traversable[A])(equivBy: A => B) extends immutable.Set[A] {
  private def newWrap(value: A) = new Wrap[A, B](value)(equivBy)
  val wrapSet = (basis map newWrap).toSet[Wrap[A, B]]

  def withEquiv[C](equivBy: A => C): CustomEqualitySet[A, C] = new CustomEqualitySet(basis)(equivBy)

  override def size              = wrapSet.size
  def iterator: Iterator[A]      = wrapSet.iterator map (_.value)
  def contains(elem: A): Boolean = wrapSet(newWrap(elem))
  def -(elem: A)                 = this
  def +(elem: A)                 = this
}
