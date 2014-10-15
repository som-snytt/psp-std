package psp
package std

import api._

/** A valid index is always non-negative. All negative indices are
 *  mapped to NoIndex, which has an underlying value of -1.
 *  In principle we could double the usable range by treating
 *  value as an unsigned Int other than -1.
 */
final class IndexImpl private[std] (val value: Int) extends AnyVal with IndexOrNth with api.Index {
  def indexValue = value
  // type This = Index

  // Manipulations of NoIndex result in NoIndex, it's like NaN in that way.
  def +(n: Int): Index = if (isUndefined) this else Index(value + n)
  def -(n: Int): Index = if (isUndefined) this else Index(value - n)

  def until(end: Index): IndexRange = IndexRange.until(this, end)
  def to(end: Index): IndexRange    = IndexRange.to(this, end)
  def take(n: Int): IndexRange      = this until this + (n max 0)

  def indicesUntil = Index.zero until this
  def indicesTo    = Index.zero to this

  // Name based extractors
  def get         = value
  def isEmpty     = isUndefined
  def isUndefined = value < 0
  def isDefined   = !isUndefined

  def spaces = " " * value
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

  def toSizeInfo: SizeInfo = toSize

  def fold[A](zero: => A)(f: Int => A): A = if (isDefined) f(value) else zero

  override def toString = s"$value"
}

object Index extends (Int => IndexImpl) {
  def max: IndexImpl       = new IndexImpl(MaxInt)
  def zero: IndexImpl      = new IndexImpl(0)
  def undefined: IndexImpl = new IndexImpl(-1)

  def fromNth(nth: Nth): IndexImpl      = apply(nth.value - 1)
  def apply(value: Int): IndexImpl      = if (value < 0) undefined else new IndexImpl(value)
  def unapply(n: IndexOrNth): IndexImpl = if (n.isUndefined) undefined else apply(n.intIndex)
}
