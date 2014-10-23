package psp
package std

import api._

object PolicySet {
  def builder[A: HashEq] : Builds[A, exSet[A]] = Builds(ExtensionalSet[A](_))

  def natural[A](xs: pSeq[A]): exSet[A]                                    = ExtensionalSet[A](xs)(HashEq.natural[A])
  def reference[A <: AnyRef](xs: pSeq[A]): exSet[A]                        = ExtensionalSet[A](xs)(HashEq.reference[A])
  def shown[A: Show](xs: pSeq[A]): exSet[A]                                = ExtensionalSet[A](xs)(HashEq.shown[A])
  def direct[A](xs: pSeq[A])(equiv: Relation[A], hash: A => Int): exSet[A] = ExtensionalSet[A](xs)(HashEq(equiv, hash))
  def elems[A: HashEq](xs: A*): exSet[A]                                   = ExtensionalSet[A](fromElems(xs: _*))

  class FromScala[A](xs: sciSet[A]) extends ExtensionalSet[A] {
    def size: IntSize     = Precise(xs.size)
    def contained         = Foreach[A](xs foreach _)
    def apply(elem: A)    = xs(elem)
    def equiv(x: A, y: A) = x == y
    def hash(x: A)        = x.##
  }
  class FromJava[A](xs: jSet[A]) extends ExtensionalSet[A] {
    def size: IntSize     = Precise(xs.size)
    def contained         = Foreach[A](BiIterable(xs) foreach _)
    def apply(elem: A)    = xs contains elem
    def equiv(x: A, y: A) = x == y
    def hash(x: A)        = x.##
  }
}

sealed trait IntensionalSet[A] extends InSet[A] with (A => Boolean) {
  def hash(x: A): Int
}
sealed trait ExtensionalSet[A] extends ExSet[A] with IntensionalSet[A]

object ExtensionalSet {
  def apply[A: HashEq](xs: pSeq[A]): exSet[A] = new Impl[A](xs, implicitly)

  sealed trait Derived[A] extends AnyRef with ExtensionalSet[A] with IntensionalSet.Derived[A] {
    protected def underlying: exSet[A]
  }
  final case class Filtered[A](lhs: exSet[A], rhs: Predicate[A]) extends Derived[A] {
    protected def underlying = lhs
    def apply(elem: A)       = lhs(elem) && !rhs(elem)
    def contained            = lhs.contained filter rhs
    def size                 = lhs.size.atMost
  }
  final case class Intersect[A](lhs: exSet[A], rhs: exSet[A]) extends Derived[A] {
    protected def underlying = lhs
    def apply(elem: A)       = lhs(elem) && rhs(elem)
    def contained            = lhs.contained filter rhs
    def size                 = lhs.size intersect rhs.size
  }
  final case class Union[A](lhs: exSet[A], rhs: exSet[A]) extends Derived[A] {
    protected def underlying = lhs
    def apply(elem: A)       = lhs(elem) || rhs(elem)
    def contained            = Foreach.join(lhs.contained, rhs.contained filterNot lhs)
    def size                 = lhs.size union rhs.size
  }
  final case class Diff[A](lhs: exSet[A], rhs: exSet[A]) extends Derived[A] {
    protected def underlying = lhs
    def apply(elem: A)       = lhs(elem) && !rhs(elem)
    def contained            = lhs.contained filterNot rhs
    def size                 = lhs.size diff rhs.size
  }
  final class Impl[A](basis: Foreach[A], heq: HashEq[A]) extends ExtensionalSet[A] {
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
    def equiv(x: A, y: A)     = heq.equiv(x, y)
    def hash(x: A): Int       = heq.hash(x)
    def apply(elem: A)        = wrapSet contains wrap(elem)
    def size: Precise         = newSize(wrapSet.size)
    def contained: Foreach[A] = wrapSet.m map (_.unwrap)
  }
}
object IntensionalSet {
  def apply[A: HashEq](p: Predicate[A]): inSet[A] = new Impl[A](p, implicitly)

  val Zero = Impl[Any](ConstantFalse, HashEq((x, y) => true, _ => 0))
  val One  = Impl[Any](ConstantTrue, HashEq((x, y) => true, _ => 0))

  sealed trait Derived[A] extends AnyRef with inSet[A] {
    protected def underlying: inSet[A]
    def equiv(x: A, y: A) = underlying.equiv(x, y)
    def hash(x: A)        = underlying.hash(x)
  }
  final case class Filtered[A](lhs: inSet[A], rhs: Predicate[A]) extends Derived[A] {
    protected def underlying = lhs
    def apply(elem: A)       = lhs(elem) && !rhs(elem)
  }
  final case class Complement[A](xs: inSet[A]) extends Derived[A] {
    protected def underlying = xs
    def apply(elem: A)       = !xs(elem)
  }
  final case class Intersect[A](lhs: inSet[A], rhs: inSet[A]) extends Derived[A] {
    protected def underlying = lhs
    def apply(elem: A)       = lhs(elem) && rhs(elem)
  }
  final case class Union[A](lhs: inSet[A], rhs: inSet[A]) extends Derived[A] {
    protected def underlying = lhs
    def apply(elem: A)       = lhs(elem) && !rhs(elem)
  }
  final case class Diff[A](lhs: inSet[A], rhs: inSet[A]) extends Derived[A] {
    protected def underlying = lhs
    def apply(elem: A)       = lhs(elem) && !rhs(elem)
  }
  final case class Impl[A](member: Predicate[A], heq: HashEq[A]) extends IntensionalSet[A] {
    def equiv(x: A, y: A) = heq.equiv(x, y)
    def hash(x: A): Int   = heq.hash(x)
    def apply(elem: A)    = member(elem)
  }
}

/*** TODO - salvage.

//   def addIfAbsent(elem: A): PolicySet[A]  = if (contains(elem)) this else this + elem
//   def addOrReplace(elem: A): PolicySet[A] = if (contains(elem)) (this - elem) + elem else this + elem

//   def by[B : HashEq](f: A => B): PolicySet[A]    = PolicySet[A](basis)(hashEqBy[A](f))
//   def byNatural                                  = PolicySet.natural[A](basis)
//   def byReference(implicit ev: A <:< AnyRef)     = PolicySet.reference(basis map ev)
//   def byShown(implicit z: Show[A]): PolicySet[A] = PolicySet.shown[A](basis)
//   implicit def scalaSetEq[CC[X] <: sciSet[X], A : Eq] : Eq[CC[A]] = Eq[CC[A]] {
//     case (Complement(xs), Complement(ys)) => isSubSet(xs, ys) && isSubSet(ys, xs)
//     case (Complement(xs), y)              => false
//     case (x, Complement(ys))              => false
//     case (xs, ys)                         => isSubSet(xs, ys) && isSubSet(ys, xs)
//   }

***/
