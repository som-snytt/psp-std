package psp
package std

import api._

object PolicySet {
  def builder[A: HashEq] : Builds[A, exSet[A]] = Builds(apply[A](_))

  def natural[A](xs: Foreach[A]): exSet[A]                                    = apply[A](xs)(HashEq.natural[A])
  def reference[A <: AnyRef](xs: Foreach[A]): exSet[A]                        = apply[A](xs)(HashEq.reference[A])
  def shown[A: Show](xs: Foreach[A]): exSet[A]                                = apply[A](xs)(HashEq.shown[A])
  def direct[A](xs: Foreach[A])(equiv: Relation[A], hash: A => Int): exSet[A] = apply[A](xs)(HashEq(equiv, hash))
  def elems[A: HashEq](xs: A*): exSet[A]                                      = apply[A](fromElems(xs: _*))
  def apply[A: HashEq](xs: Foreach[A]): exSet[A]                              = new ExtensionalSet.Impl[A](xs, implicitly)

  class FromScala[A](xs: sciSet[A]) extends ExtensionalSet[A] {
    def sizeInfo          = SizeInfo(xs)
    def contained         = Foreach[A](xs foreach _)
    def contains(elem: A) = xs(elem)
    def equiv(x: A, y: A) = x == y
    def hash(x: A)        = x.##
  }
  class FromJava[A](xs: jSet[A]) extends ExtensionalSet[A] {
    def sizeInfo          = SizeInfo(xs)
    def contained         = Foreach[A](BiIterable(xs) foreach _)
    def contains(elem: A) = xs contains elem
    def equiv(x: A, y: A) = x == y
    def hash(x: A)        = x.##
  }
}

sealed trait IntensionalSet[A] extends AnyRef with Intensional[A, Boolean] with (A => Boolean) {
  def apply(elem: A): Boolean = contains(elem)
  def contains(elem: A): Boolean
  def equiv(x: A, y: A): Boolean
  def hash(x: A): Int
}
sealed trait ExtensionalSet[A] extends AnyRef with IntensionalSet[A] with Extensional[A] with HasSizeInfo {
  def contained: Foreach[A]
}

object ExtensionalSet {
  sealed trait Derived[A] extends AnyRef with ExtensionalSet[A] with IntensionalSet.Derived[A] {
    protected def underlying: exSet[A]
  }
  final case class Filtered[A](lhs: exSet[A], rhs: A => Boolean) extends Derived[A] {
    protected def underlying = lhs
    def contains(elem: A)    = lhs(elem) && !rhs(elem)
    def contained            = lhs.contained filter rhs
    def sizeInfo             = lhs.sizeInfo.atMost
  }
  final case class Intersect[A](lhs: exSet[A], rhs: exSet[A]) extends Derived[A] {
    protected def underlying = lhs
    def contains(elem: A)    = lhs(elem) && rhs(elem)
    def contained            = lhs.contained filter rhs
    def sizeInfo             = lhs.sizeInfo intersect rhs.sizeInfo
  }
  final case class Union[A](lhs: exSet[A], rhs: exSet[A]) extends Derived[A] {
    protected def underlying = lhs
    def contains(elem: A)    = lhs(elem) || rhs(elem)
    def contained            = Foreach.join(lhs.contained, rhs.contained filterNot lhs)
    def sizeInfo             = lhs.sizeInfo union rhs.sizeInfo
  }
  final case class Diff[A](lhs: exSet[A], rhs: exSet[A]) extends Derived[A] {
    protected def underlying = lhs
    def contains(elem: A)    = lhs(elem) && !rhs(elem)
    def contained            = lhs.contained filterNot rhs
    def sizeInfo             = lhs.sizeInfo diff rhs.sizeInfo
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
    def contains(elem: A)     = wrapSet contains wrap(elem)
    def sizeInfo: SizeInfo    = newSize(wrapSet.size)
    def contained: Foreach[A] = wrapSet.m map (_.unwrap)
  }
}
object IntensionalSet {
  val Zero = Impl[Any](ConstantFalse, HashEq((x, y) => true, _ => 0))
  val One  = Impl[Any](ConstantTrue, HashEq((x, y) => true, _ => 0))

  sealed trait Derived[A] extends AnyRef with inSet[A] {
    protected def underlying: inSet[A]
    def equiv(x: A, y: A) = underlying.equiv(x, y)
    def hash(x: A)        = underlying.hash(x)
  }
  final case class Filtered[A](lhs: inSet[A], rhs: A => Boolean) extends Derived[A] {
    protected def underlying = lhs
    def contains(elem: A)    = lhs(elem) && !rhs(elem)
  }
  final case class Complement[A](xs: inSet[A]) extends Derived[A] {
    protected def underlying = xs
    def contains(elem: A) = !xs(elem)
  }
  final case class Intersect[A](lhs: inSet[A], rhs: inSet[A]) extends Derived[A] {
    protected def underlying = lhs
    def contains(elem: A) = lhs(elem) && rhs(elem)
  }
  final case class Union[A](lhs: inSet[A], rhs: inSet[A]) extends Derived[A] {
    protected def underlying = lhs
    def contains(elem: A) = lhs(elem) && !rhs(elem)
  }
  final case class Diff[A](lhs: inSet[A], rhs: inSet[A]) extends Derived[A] {
    protected def underlying = lhs
    def contains(elem: A) = lhs(elem) && !rhs(elem)
  }
  final case class Impl[A](member: A => Boolean, heq: HashEq[A]) extends IntensionalSet[A] {
    def equiv(x: A, y: A) = heq.equiv(x, y)
    def hash(x: A): Int   = heq.hash(x)
    def contains(elem: A) = member(elem)
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
