package psp
package std

import api._

trait HashEq[-A] extends Any with Hash[A] with Eq[A]

object Hash {
  final class Impl[-A](val f: A => Int) extends AnyVal with Hash[A] { def hash(x: A): Int = f(x) }

  def reference[A](): Impl[A with AnyRef] = new Impl[A with AnyRef](_.id_##)
  def natural[A](): Impl[A]               = new Impl[A](_.##)
  def apply[A](f: A => Int): Impl[A]      = new Impl[A](f)
}

object Eq {
  final class Impl[-A](val f: (A, A) => Boolean) extends AnyVal with Eq[A] { def equiv(x: A, y: A) = f(x, y) }

  def reference[A](): Impl[A with AnyRef]     = new Impl[A with AnyRef](_ id_== _)
  def natural[A](): Impl[A]                   = new Impl[A](_ == _)
  def apply[A](f: (A, A) => Boolean): Impl[A] = new Impl[A](f)
}
object HashEq {
  implicit def composeHashEq[A](implicit eqs: Eq[A], hash: Hash[A]): HashEq[A] = new Impl[A](eqs.equiv, hash.hash)

  def apply[A](cmp: (A, A) => Boolean, hashFn: A => Int): Impl[A] = new Impl[A](cmp, hashFn)

  def natural[A](eqs: Eq[A]): Impl[A]   = apply[A](eqs.equiv, _.##)
  def natural[A](): Impl[A]             = apply[A](_ == _, _.##)
  def reference[A <: AnyRef](): Impl[A] = apply[A](_ eq _, identityHashCode)
  def shown[A: Show](): Impl[A]         = apply[A](_.to_s == _.to_s, _.to_s.##)

  final case class Wrap[A: HashEq](value: A) {
    override def hashCode = value.hash
    override def equals(x: Any): Boolean = x match {
      case Wrap(that) => value === that.castTo[A]
      case _          => false
    }
    override def toString = pp"$value"
  }
  final class Impl[-A](isEquiv: (A, A) => Boolean, hashFn: A => Int) extends HashEq[A] {
    def equiv(x: A, y: A) = isEquiv(x, y)
    def hash(x: A)        = hashFn(x)
  }
}
