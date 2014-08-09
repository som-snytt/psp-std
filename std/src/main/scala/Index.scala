package psp
package std

import Index.{ zero, empty }

/** A valid index is always non-negative. All negative indices are
 *  mapped to NoIndex, which has an underlying value of -1.
 *  In principle we could double the usable range by treating
 *  value as an unsigned Int other than -1.
 */
final class Index private (val value: Int) extends AnyVal with Ordered[Index] {
  // Manipulations of NoIndex result in NoIndex, it's like NaN in that way.
  def +(n: Int): Index = if (isDefined) Index(value + n) else NoIndex
  def -(n: Int): Index = if (isDefined) Index(value - n) else NoIndex

  def until(end: Index): IndexRange = IndexRange.until(value, end.value)
  def to(end: Index): IndexRange    = IndexRange.to(value, end.value)

  def indicesDownToZero = value to 0 by -1 map Index
  def indicesUntil      = zero until this
  def indicesTo         = zero to this

  def isDefined   = this != empty
  def isUndefined = this == empty

  def spaces                    = " " * value
  def compare(that: Index): Int = value compare that.value

  def prev = this - 1
  def next = this + 1
  def max(that: Index): Index = Index(value max that.value)
  def min(that: Index): Index = Index(value min that.value)

  def toInt: Int   = value
  def toLong: Long = value.toLong
  def toNth: Nth   = Nth fromIndex this

  override def toString         = s"$value"
}

object Index extends (Int => Index) {
  def fromNth(nth: Nth): Index = if (nth.isDefined) apply(nth.value - 1) else empty
  def zero: Index  = new Index(0)
  def empty: Index = new Index(-1)
  def apply(value: Int): Index = if (value < 0) empty else new Index(value)
}
