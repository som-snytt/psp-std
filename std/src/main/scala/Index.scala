package psp
package std

import Index.{ zero, undefined }

/** A valid index is always non-negative. All negative indices are
 *  mapped to NoIndex, which has an underlying value of -1.
 *  In principle we could double the usable range by treating
 *  value as an unsigned Int other than -1.
 */
final class Index private (val value: Int) extends AnyVal with Ordered[Index] with IndexOrNth {
  type This = Index

  // Manipulations of NoIndex result in NoIndex, it's like NaN in that way.
  def +(n: Int): Index = if (isUndefined) this else Index(value + n)
  def -(n: Int): Index = if (isUndefined) this else Index(value - n)

  def until(end: Index): IndexRange = IndexRange.until(this, end)
  def to(end: Index): IndexRange    = IndexRange.to(this, end)
  def take(n: Int): IndexRange      = this until this + (n max 0)

  def indicesUntil = zero until this
  def indicesTo    = zero to this

  // Name based extractors
  def get         = value
  def isEmpty     = isUndefined
  def isUndefined = this == undefined

  def spaces                    = " " * value
  def compare(that: Index): Int = value compare that.value

  def prev = this - 1
  def next = this + 1
  def max(that: Index): Index = Index(value max that.value)
  def min(that: Index): Index = Index(value min that.value)

  def toInt: Int     = value
  def toLong: Long   = value.toLong
  def toNth: Nth     = Nth fromIndex this
  def toIndex: Index = this
  def intIndex: Int  = value
  def intNth: Int    = toNth.value
  def toSize: Size   = Size(value)

  override def toString         = s"$value"
}

object Index extends (Int => Index) {
  def max: Index       = new Index(Int.MaxValue)
  def zero: Index      = new Index(0)
  def undefined: Index = new Index(-1)

  def fromNth(nth: Nth): Index      = apply(nth.value - 1)
  def apply(value: Int): Index      = if (value < 0) undefined else new Index(value)
  def unapply(n: IndexOrNth): Index = if (n.isUndefined) undefined else apply(n.intIndex)
}
