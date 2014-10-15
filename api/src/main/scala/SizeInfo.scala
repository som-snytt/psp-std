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
sealed trait SizeInfo

// This arrangement means that "x + size" results in the same type as x for any SizeInfo.
sealed trait Atomic                            extends SizeInfo
final case class Bounded(lo: Size, hi: Atomic) extends SizeInfo
final case class Precise(sizeValue: Int)       extends Atomic with Size {
  def isEmpty                   = sizeValue < 0
  def get                       = sizeValue
  def +(that: Precise): Precise = Precise(sizeValue + that.sizeValue)
}
final case object Infinite                     extends Atomic

trait HasSizeInfo extends Any { def sizeInfo: SizeInfo }
trait HasPreciseSize extends Any with HasSizeInfo with IsEmpty {
  def size: Precise
  def sizeInfo = size
  def isEmpty  = size.sizeValue == 0
}
