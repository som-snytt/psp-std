package psp
package std

import Nth.{ empty }

/** A one-based index.
 */
final class Nth private (val value: Int) extends AnyVal with Ordered[Nth] {
  def +(n: Int): Nth = if (isDefined) Nth(value + n) else NoNth
  def -(n: Int): Nth = if (isDefined) Nth(value - n) else NoNth

  def until(end: Nth): IndexRange = toIndex until end.toIndex
  def to(end: Nth): IndexRange    = toIndex to end.toIndex
  def take(n: Int): IndexRange    = toIndex take n

  def isDefined = this != empty
  def compare(that: Nth): Int = value compare that.value

  def prev = this - 1
  def next = this + 1
  def max(that: Index): Index = Index(value max that.value)
  def min(that: Index): Index = Index(value min that.value)

  def toInt: Int     = value
  def toLong: Long   = value.toLong
  def toNth: Nth     = this
  def toIndex: Index = Index fromNth this
  def intIndex: Int  = toIndex.value

  override def toString = if (isDefined) s"Nth($value)" else "Nth.empty"
}
object Nth extends (Int => Nth) {
  // 0 is excluded, but we use -1 for the empty case anyway.
  def empty = new Nth(-1)

  def fromIndex(index: Index): Nth = apply(index.value + 1)
  def apply(value: Int): Nth       = if (value < 1) empty else new Nth(value)
}
