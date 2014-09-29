package psp
package std
package api

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
sealed trait SizeInfo

// This arrangement means that "x + size" results in the same type as x for any SizeInfo.
sealed trait Atomic                            extends SizeInfo
final case class Bounded(lo: Size, hi: Atomic) extends SizeInfo
final case class Precise(size: Size)           extends Atomic with Size { def value: Int = size.value }
final case object Infinite                     extends Atomic

trait HasSizeInfo extends Any                        { def sizeInfo: SizeInfo }
trait HasPreciseSize extends Any with HasSizeInfo    { def size: Size         }
trait HasStaticSize[N <: Nat] extends Any with HasPreciseSize

trait Nat extends Any
