package psp
package std
package core

// import psp.std._
import scala.collection.{ mutable, immutable }

object EquivSet {
  def universal[A](xs: Foreach[A]): EquivSet[A]           = apply[A](xs)(HashEq.universal[A])
  def reference[A <: AnyRef](xs: Foreach[A]): EquivSet[A] = apply[A](xs)(HashEq.reference[A])
  def shown[A: Show](xs: Foreach[A]): EquivSet[A]         = apply[A](xs)(HashEq.shown[A])
  def apply[A: HashEq](xs: Foreach[A]): EquivSet[A]       = new EquivSet[A](xs)
}

final class EquivSet[A : HashEq](basis: Foreach[A]) extends immutable.Set[A] {
  private def hasheq              = implicitly[HashEq[A]]
  private[this] val wrapSet       = basis map wrap toScalaSet
  private def wrap(elem: A): Wrap = new Wrap(elem)
  private class Wrap(val unwrap: A) {
    final override def equals(that: Any): Boolean = that match {
      case x: Wrap => unwrap === x.unwrap
      case _       => false
    }
    override def hashCode = hasheq hash unwrap
    override def toString = s"$unwrap (wrapped)"
  }

  def byUniversal = EquivSet[A](basis)(HashEq.universal)
  def byReference = EquivSet[A with AnyRef](basis map (_.castTo[A with AnyRef]))(HashEq.reference)
  def byShown     = EquivSet[A](basis)(HashEq shown Show.native[A])

  def grouped = wrapSet.toList groupBy (x => x) map { case (k, vs) => (k.unwrap, vs map (_.unwrap)) }

  override def size              = wrapSet.size
  def iterator: Iterator[A]      = wrapSet.iterator map (_.unwrap)
  def contains(elem: A): Boolean = wrapSet(wrap(elem))
  def -(elem: A)                 = this
  def +(elem: A)                 = this
}
