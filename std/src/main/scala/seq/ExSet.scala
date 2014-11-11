package psp
package std

import api._

object ExSet {
  def apply[A: HashEq](xs: Each[A]): ExSet[A]                              = new Impl[A](xs, ?)
  def builder[A: HashEq] : Builds[A, ExSet[A]]                             = Builds(ExSet[A](_))
  def natural[A](xs: Each[A]): ExSet[A]                                    = apply[A](xs)(HashEq.natural[A])
  def reference[A <: AnyRef](xs: Each[A]): ExSet[A]                        = apply[A](xs)(HashEq.reference[A])
  def shown[A: Show](xs: Each[A]): ExSet[A]                                = apply[A](xs)(HashEq.shown[A])
  def direct[A](xs: Each[A])(equiv: Relation[A], hash: A => Int): ExSet[A] = apply[A](xs)(HashEq(equiv, hash))
  def elems[A: HashEq](xs: A*): ExSet[A]                                   = apply[A](Direct(xs: _*))

  def fromJava[A](xs: jSet[A]): ExSet[A]   = new FromJava(xs)
  def fromScala[A](xs: scSet[A]): ExSet[A] = new FromScala(xs.toSet)

  def impl[A](xs: ExSet[A]): Impl[A] = xs match {
    case xs: Impl[A] => xs
    case _           => apply[A](xs)(xs.hashEq)
  }

  sealed trait ExSetImpl[A] extends ExSet[A] with Predicate[A] {
    def hash(x: A): Int             = hashEq hash x
    def equiv(x: A, y: A): Boolean  = hashEq.equiv(x, y)
  }

  class FromScala[A](xs: sciSet[A]) extends ExSetImpl[A] {
    def size: IntSize                 = Precise(xs.size)
    @inline def foreach(f: A => Unit) = xs foreach f
    def apply(elem: A)                = xs(elem)
    def hashEq                        = HashEq.natural()
  }
  class FromJava[A](xs: jSet[A]) extends ExSetImpl[A] {
    def size: IntSize                 = Precise(xs.size)
    @inline def foreach(f: A => Unit) = xs.iterator foreach f
    def apply(elem: A)                = xs contains elem
    def hashEq                        = HashEq.natural()
  }

  sealed trait Derived[A] extends ExSetImpl[A] {
    protected def underlying: ExSet[A]
    def hashEq = underlying.hashEq
  }
  final case class Filtered[A](lhs: ExSet[A], p: Predicate[A]) extends Derived[A] {
    protected def underlying                = lhs
    def apply(elem: A)                      = lhs(elem) && p(elem)
    @inline def foreach(f: A => Unit): Unit = lhs foreach (x => if (p(x)) f(x))
    def size                                = lhs.size.atMost
  }
  final case class Intersect[A](lhs: ExSet[A], rhs: ExSet[A]) extends Derived[A] {
    protected def underlying                = lhs
    def apply(elem: A)                      = lhs(elem) && rhs(elem)
    @inline def foreach(f: A => Unit): Unit = lhs filter rhs foreach f
    def size                                = lhs.size intersect rhs.size
  }
  final case class Union[A](lhs: ExSet[A], rhs: ExSet[A]) extends Derived[A] {
    protected def underlying                = lhs
    def apply(elem: A)                      = lhs(elem) || rhs(elem)
    @inline def foreach(f: A => Unit): Unit = Each.join(lhs, rhs filterNot lhs) foreach f
    def size                                = lhs.size union rhs.size
  }
  final case class Diff[A](lhs: ExSet[A], rhs: ExSet[A]) extends Derived[A] {
    protected def underlying                = lhs
    def apply(elem: A)                      = lhs(elem) && !rhs(elem)
    @inline def foreach(f: A => Unit): Unit = lhs filterNot rhs foreach f
    def size                                = lhs.size diff rhs.size
  }
  final class Impl[A](basis: Each[A], val hashEq: HashEq[A]) extends ExSetImpl[A] {
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
    def apply(elem: A)                      = wrapSet contains wrap(elem)
    def size: Precise                       = wrapSet.size.size
    @inline def foreach(f: A => Unit): Unit = wrapSet foreach (x => f(x.unwrap))
  }
}

object InSet {
  val Zero = Pure[Any](ConstantFalse)
  val One  = Pure[Any](ConstantTrue)

  def impl[A](xs: InSet[A]): Impl[A] = xs match {
    case xs: Impl[A] => xs
    case _           => apply[A](xs)
  }
  def apply[A](p: Predicate[A]): InSet[A] = p match {
    case ConstantFalse => Zero
    case ConstantTrue  => One
    case _             => Pure[A](p)
  }
  def show[A](xs: InSet[A])(implicit z: Show[InSet[A]] = shower[A]): String = xs match {
    case Zero                => "∅"
    case One                 => "U"
    case Complement(xs)      => show"$xs′"
    case Intersect(lhs, rhs) => show"$lhs ∩ $rhs"
    case Union(lhs, rhs)     => show"$lhs ∪ $rhs"
    case Diff(lhs, rhs)      => show"$lhs ∖ $rhs"
    case Pure(f: ShowDirect) => f.to_s
    case _                   => "{ ... }"
  }

  abstract class Impl[A](p: Predicate[A])                     extends InSet[A] with Predicate[A] { def apply(x: A) = p(x) ; override def toString = show(this) }
  final case class Complement[A](lhs: InSet[A])               extends Impl[A](x => !lhs(x))
  final case class Intersect[A](lhs: InSet[A], rhs: InSet[A]) extends Impl[A](x => lhs(x) && rhs(x))
  final case class Union[A](lhs: InSet[A], rhs: InSet[A])     extends Impl[A](x => lhs(x) || rhs(x))
  final case class Diff[A](lhs: InSet[A], rhs: InSet[A])      extends Impl[A](x => lhs(x) && !rhs(x))
  final case class Pure[A](p: Predicate[A])                   extends Impl[A](p)

  private def shower[A]: Show[InSet[A]] = Show(show[A])
}
