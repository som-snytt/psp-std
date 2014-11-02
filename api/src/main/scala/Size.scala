package psp
package std
package api

import scala._

/** The Size hierarchy is:
 *                   Size
 *                  /        \
 *               Atomic      Bounded
 *              /      \
 *          Infinite  Precise
 *
 *  Precise implies the exact size is known. Infinite means it's infinite.
 *  Bounded is a size lower bound and a (possibly infinite) atomic upper bound.
 *  Size forms a partial order, with some liberties taken at present.
 *  Operations on sizes which are ill-defined will result in "Unknown", which
 *  encodes no available size information: Bounded(Zero, Infinite).
 *
 *  Invariants:
 *  - Precise is non-negative
 */

sealed trait Size extends Any
sealed trait Atomic extends Any with Size
sealed trait Precise extends Any with Atomic {
  def value: Long
  override def toString = s"$value"
}

final case class Bounded private[api] (lo: Precise, hi: Atomic) extends Size
final case class LongSize private[api] (value: Long) extends AnyVal with Precise
final case class IntSize private[api] (intValue: Int) extends AnyVal with Precise { def value = intValue }
final case object Infinite extends Atomic

trait HasSize        extends Any                           { def size: Size     }
trait HasAtomicSize  extends Any with HasSize with IsEmpty { def size: Atomic   }
trait HasPreciseSize extends Any with HasAtomicSize        { def size: Precise  }
trait HasIntSize     extends Any with HasPreciseSize       { def size: IntSize  }

object Size {
  /** Preserving associativity/commutativity of Size prevents us from
   *  modifying values to enforce any invariants on Bounded.
   */
  def apply(size: Long): LongSize             = LongSize( if (size < 0L) 0L else size )
  def apply(size: Int): IntSize               = IntSize( if (size < 0) 0 else size )
  def apply(lo: Precise, hi: Atomic): Bounded = Bounded(lo, hi)
}

object Precise {
  def apply(size: Long): LongSize     = Size(size)
  def apply(size: Int): IntSize       = Size(size)
  def unapply(x: Precise): Some[Long] = Some(x.value) // XXX 2.11 this should be made box-free
}
object PreciseInt {
  def unapply(x: Precise): Option[Int] = if (x.value <= Int.MaxValue.toLong) Some(x.value.toInt) else None
}
object HasSize {
  def unapply(x: HasSize): Some[Size] = Some(x.size)
}
