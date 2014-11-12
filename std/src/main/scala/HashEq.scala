package psp
package std

import api._

/** A bad idea in general, but so much less ceremony for limted-use classes.
 */
trait NaturalHashEq

object Hash {
  final class Impl[-A](val f: A => Int) extends AnyVal with Hash[A] { def hash(x: A): Int = f(x) }

  def reference[A](): Hash[Ref[A]]   = new Impl[Ref[A]](_.id_##)
  def natural[A](): Hash[A]          = new Impl[A](_.##)
  def apply[A](f: A => Int): Hash[A] = new Impl[A](f)
}

object Eq {
  final class Impl[-A](val f: Relation[A]) extends AnyVal with Eq[A] { def equiv(x: A, y: A) = f(x, y) }

  def reference[A](): Eq[Ref[A]]      = new Impl[Ref[A]](_ id_== _)
  def natural[A](): Eq[A]             = new Impl[A](_ == _)
  def apply[A](f: Relation[A]): Eq[A] = new Impl[A](f)
}

trait HashEqLow {
  implicit def universalEq[A <: NaturalHashEq] : HashEq[A] = HashEq.natural()
}

object HashEq extends HashEqLow {
  implicit def composeHashEq[A](implicit eqs: Eq[A], hash: Hash[A]): HashEq[A] = new Impl[A](eqs.equiv, hash.hash)

  def apply[A](cmp: Relation[A], hashFn: A => Int): HashEq[A] = new Impl[A](cmp, hashFn)

  def natural[A](eqs: Eq[A]): HashEq[A]   = apply[A](eqs.equiv, _.##)
  def natural[A](): HashEq[A]             = apply[A](_ == _, _.##)
  def reference[A <: AnyRef](): HashEq[A] = apply[A](_ eq _, identityHashCode)
  def shown[A: Show](): HashEq[A]         = apply[A](_.to_s == _.to_s, _.to_s.##)

  final case class Wrap[A: HashEq](value: A) {
    override def hashCode = value.hash
    override def equals(x: Any): Boolean = x match {
      case Wrap(that) => value === that.castTo[A]
      case _          => false
    }
    override def toString = pp"$value"
  }
  final class Impl[-A](isEquiv: Relation[A], hashFn: A => Int) extends HashEq[A] {
    def equiv(x: A, y: A) = isEquiv(x, y)
    def hash(x: A)        = hashFn(x)
  }
}
