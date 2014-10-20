package psp
package std
package api

import scala._

/** The SizeInfo hierarchy is:
 *                   SizeInfo
 *                  /        \
 *               Atomic      Bounded
 *              /      \
 *          Infinite  Precise
 *
 *  Precise implies the exact size is known. Infinite means it's infinite.
 *  Bounded is a size lower bound and a (possibly infinite) atomic upper bound.
 *  SizeInfo forms a partial order, with some liberties taken at present.
 *  Operations on sizes which are ill-defined will result in "Unknown", which
 *  encodes no available size information: Bounded(Zero, Infinite).
 */

sealed trait SizeInfo extends Any
sealed trait Atomic extends Any with SizeInfo

final case class Bounded(lo: PreciseSize, hi: Atomic) extends SizeInfo
final case class PreciseSize private (value: Long) extends AnyVal with Atomic { override def toString = s"$value" }
final case object Infinite extends Atomic

trait HasSizeInfo extends Any                       { def sizeInfo: SizeInfo    }
trait HasAtomicSize extends Any with IsEmpty        { def sizeInfo: Atomic      }
trait HasPreciseSize extends Any with HasAtomicSize { def sizeInfo: PreciseSize }

object PreciseSize {
  def create(size: Long): PreciseSize = PreciseSize( if (size < 0L) 0L else size )
}
