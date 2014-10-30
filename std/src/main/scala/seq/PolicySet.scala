package psp
package std

import api._

sealed trait IntensionalSet[A] extends InSet[A] with (A => Boolean)
sealed trait ExtensionalSet[A] extends IntensionalSet[A] with ExSet[A] {
  def hash(x: A): Int             = hashEq hash x
  def equiv(x: A, y: A): Boolean  = hashEq.equiv(x, y)
}

object PolicySet {
  def builder[A: HashEq] : Builds[A, ExSet[A]]                             = Builds(ExtensionalSet[A](_))
  def natural[A](xs: Each[A]): ExSet[A]                                    = ExtensionalSet[A](xs)(HashEq.natural[A])
  def reference[A <: AnyRef](xs: Each[A]): ExSet[A]                        = ExtensionalSet[A](xs)(HashEq.reference[A])
  def shown[A: Show](xs: Each[A]): ExSet[A]                                = ExtensionalSet[A](xs)(HashEq.shown[A])
  def direct[A](xs: Each[A])(equiv: Relation[A], hash: A => Int): ExSet[A] = ExtensionalSet[A](xs)(HashEq(equiv, hash))
  def elems[A: HashEq](xs: A*): ExSet[A]                                   = ExtensionalSet[A](Direct(xs: _*))

  class FromScala[A](xs: sciSet[A]) extends ExtensionalSet[A] {
    def size: IntSize         = Precise(xs.size)
    def foreach(f: A => Unit) = xs foreach f
    def apply(elem: A)        = xs(elem)
    def hashEq                = HashEq.natural()
  }
  class FromJava[A](xs: jSet[A]) extends ExtensionalSet[A] {
    def size: IntSize         = Precise(xs.size)
    def foreach(f: A => Unit) = xs.iterator foreach f
    def apply(elem: A)        = xs contains elem
    def hashEq                = HashEq.natural()
  }
}

object ExtensionalSet {
  def apply[A: HashEq](xs: Each[A]): ExSet[A] = new Impl[A](xs, implicitly)

  sealed trait Derived[A] extends ExtensionalSet[A] {
    protected def underlying: ExSet[A]
    def hashEq = underlying.hashEq
  }
  final case class Filtered[A](lhs: ExSet[A], p: Predicate[A]) extends Derived[A] {
    protected def underlying        = lhs
    def apply(elem: A)              = lhs(elem) && p(elem)
    def foreach(f: A => Unit): Unit = lhs foreach (x => if (p(x)) f(x))
    def size                        = lhs.size.atMost
  }
  final case class Intersect[A](lhs: ExSet[A], rhs: ExSet[A]) extends Derived[A] {
    protected def underlying        = lhs
    def apply(elem: A)              = lhs(elem) && rhs(elem)
    def foreach(f: A => Unit): Unit = lhs filter rhs foreach f
    def size                        = lhs.size intersect rhs.size
  }
  final case class Union[A](lhs: ExSet[A], rhs: ExSet[A]) extends Derived[A] {
    protected def underlying        = lhs
    def apply(elem: A)              = lhs(elem) || rhs(elem)
    def foreach(f: A => Unit): Unit = Each.join(lhs, rhs filterNot lhs) foreach f
    def size                        = lhs.size union rhs.size
  }
  final case class Diff[A](lhs: ExSet[A], rhs: ExSet[A]) extends Derived[A] {
    protected def underlying        = lhs
    def apply(elem: A)              = lhs(elem) && !rhs(elem)
    def foreach(f: A => Unit): Unit = lhs filterNot rhs foreach f
    def size                        = lhs.size diff rhs.size
  }
  final class Impl[A](basis: Each[A], val hashEq: HashEq[A]) extends ExtensionalSet[A] {
    private[this] val wrapSet: jSet[Wrap] = basis map wrap toJavaSet
    private def wrap(elem: A): Wrap = new Wrap(elem)
    private class Wrap(val unwrap: A) {
      final override def equals(that: Any): Boolean = that match {
        case x: Wrap => equiv(unwrap, x.unwrap)
        case _       => false
      }
      override def hashCode = hash(unwrap)
      override def toString = s"$unwrap (wrapped)"
    }
    def apply(elem: A)     = wrapSet contains wrap(elem)
    def size: Precise      = newSize(wrapSet.size)
    def foreach(f: A => Unit): Unit  = wrapSet foreach (x => f(x.unwrap))
  }
}
object IntensionalSet {
  def apply[A](p: Predicate[A]): InSet[A] = new Impl[A](p)

  val Zero = apply[Any](ConstantFalse)
  val One  = apply[Any](ConstantTrue)

  abstract class IntensionalImpl[A](p: Predicate[A]) extends IntensionalSet[A] { def apply(x: A) = p(x) }

  final case class Filtered[A](lhs: InSet[A], p: Predicate[A]) extends IntensionalImpl[A](x => lhs(x) && p(x))
  final case class Complement[A](lhs: InSet[A])                extends IntensionalImpl[A](x => !lhs(x))
  final case class Intersect[A](lhs: InSet[A], rhs: InSet[A])  extends IntensionalImpl[A](x => lhs(x) && rhs(x))
  final case class Union[A](lhs: InSet[A], rhs: InSet[A])      extends IntensionalImpl[A](x => lhs(x) || rhs(x))
  final case class Diff[A](lhs: InSet[A], rhs: InSet[A])       extends IntensionalImpl[A](x => lhs(x) && !rhs(x))
  final case class Impl[A](isMember: Predicate[A])             extends IntensionalImpl[A](isMember)
}
