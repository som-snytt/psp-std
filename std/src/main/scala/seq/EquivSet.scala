package psp
package std

import scala.collection.immutable

object EquivSet {
  implicit def equivSetEq[A: HashEq] = Eq[EquivSet[A]]((xs, ys) => (xs.size == ys.size) && (xs forall ys))
  implicit def newBuilder[A: HashEq] : Builds[A, EquivSet[A]] = Builds(xs => new EquivSet[A](xs))

  def universal[A](xs: Foreach[A]): EquivSet[A]           = apply[A](xs)(HashEq.universal[A])
  def reference[A <: AnyRef](xs: Foreach[A]): EquivSet[A] = apply[A](xs)(HashEq.reference[A])
  def shown[A: Show](xs: Foreach[A]): EquivSet[A]         = apply[A](xs)(HashEq.shown[A])
  def apply[A: HashEq](xs: Foreach[A]): EquivSet[A]       = new EquivSet[A](xs)
}

final class EquivSet[A : HashEq](basis: Foreach[A]) extends immutable.Set[A] {
  private def hasheq: HashEq[A]   = ?
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

object ScalaSet {
  private val Zero: ScalaSet[Any] = new EmptySet[Any]
  private val One: ScalaSet[Any]  = new Complement[Any](Zero)
  private class EmptySet[A] extends ScalaSet[A] {
    def iterator          = Iterator[A]()
    def -(elem: A)        = this
    def +(elem: A)        = Set(elem)
    def contains(elem: A) = false
    override def toString = "Ø"
  }

  type ScalaSet[A] = scala.collection.immutable.Set[A]

  def isSubSet[A: Eq](xs: Set[A], ys: Set[A]): Boolean = xs forall (x => ys exists (y => x === y))

  implicit def scalaSetEq[CC[X] <: Set[X], A : Eq] : Eq[CC[A]] = Eq[CC[A]] {
    case (Complement(xs), Complement(ys)) => isSubSet(xs, ys) && isSubSet(ys, xs)
    case (Complement(xs), y)              => false
    case (x, Complement(ys))              => false
    case (xs, ys)                         => isSubSet(xs, ys) && isSubSet(ys, xs)
  }

  class Algebra[A] extends BooleanAlgebra[ScalaSet[A]] {
    def and(x: ScalaSet[A], y: ScalaSet[A]): ScalaSet[A] = (x, y) match {
      case (Complement(xs), Complement(ys)) => !(xs || ys)
      case (Complement(xs), ys)             => Difference(ys, xs)
      case (xs, Complement(ys))             => Difference(xs, ys)
      case _                                => x intersect y
    }
    def or(x: ScalaSet[A], y: ScalaSet[A]): ScalaSet[A] = (x, y) match {
      case (Complement(xs), Complement(ys)) => !(xs && ys)
      case (Complement(xs), ys)             => not(Difference(xs, ys))
      case (xs, Complement(ys))             => not(Difference(ys, xs))
      case _                                => x union y
    }
    def not(x: ScalaSet[A]): ScalaSet[A] = x match {
      case Zero           => one
      case One            => zero
      case Complement(xs) => xs            // unwrap
      case _              => Complement(x) // wrap
    }
    def zero: ScalaSet[A] = Zero.castTo[ScalaSet[A]]
    def one: ScalaSet[A]  = One.castTo[ScalaSet[A]]
  }

  final case class Difference[A](lhs: Set[A], rhs: Set[A]) extends ScalaSet[A] {
    def iterator           = lhs.iterator filterNot rhs
    def -(elem: A): Set[A] = if (lhs(elem)) Difference(lhs, rhs + elem) else this
    def +(elem: A): Set[A] = if (lhs(elem) && !rhs(elem)) this else Difference(lhs + elem, rhs - elem)
    def contains(elem: A)  = lhs(elem) && !rhs(elem)
    override def toString = lhs match {
      case Complement(_) => s"($lhs) / $rhs"
      case _             => (lhs filterNot rhs).toString
    }
  }
  final case class Complement[A](xs: Set[A]) extends ScalaSet[A] {
    // The repl calls size to see if it needs to truncate the string, and then
    // the size implementation calls iterator.
    override def size      = Int.MaxValue
    def iterator           = sys.error("Cannot iterate over set complement")
    def -(elem: A): Set[A] = !(xs + elem)
    def +(elem: A): Set[A] = !(xs - elem)
    def contains(elem: A)  = !(xs contains elem)

    // It's impossible to meet the hashcode contract.
    override def hashCode = ~(xs.##)
    override def equals(x: Any): Boolean = x match {
      case Complement(ys) => xs == ys
      case _              => false
    }
    override def toString = xs match {
      case Zero => "U"
      case _    => s"U ∖ $xs"
    }
  }
}
