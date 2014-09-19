package psp
package std

import Nth.undefined

/** A one-based index. In principle Index(n) and Nth(n + 1) have the
 *  same behavior for all values n >= 0, assuming things type check.
 *  I resist cosmetic implicits.
 */
final class Nth private (val value: Int) extends AnyVal with IndexOrNth {
  type This = Nth

  def +(n: Int): Nth = if (isUndefined) this else Nth(value + n)
  def -(n: Int): Nth = if (isUndefined) this else Nth(value - n)

  def until(end: Nth): IndexRange = toIndex until end.toIndex
  def to(end: Nth): IndexRange    = toIndex to end.toIndex
  def take(n: Int): IndexRange    = toIndex take n

  def get         = value
  def isEmpty     = isUndefined
  def isUndefined = this == undefined

  def prev = this - 1
  def next = this + 1
  def max(that: Index): Index = Index(value max that.value)
  def min(that: Index): Index = Index(value min that.value)

  def toInt: Int     = value
  def toLong: Long   = value.toLong
  def toNth: Nth     = this
  def toIndex: Index = Index fromNth this
  def intIndex: Int  = toIndex.value
  def intNth: Int    = value

  override def toString = if (isUndefined) "Nth.undefined" else s"Nth($value)"
}
object Nth extends (Int => Nth) {
  implicit def nthToIndex(n: Nth): Index = n.toIndex
  implicit def NthOrder                  = orderBy[Nth](_.value)

  // 0 is excluded, but we use -1 for the undefined case anyway.
  def undefined                    = new Nth(-1)
  def fromIndex(index: Index): Nth = apply(index.value + 1)
  def apply(value: Int): Nth       = if (value < 1) undefined else new Nth(value)
  def unapply(n: IndexOrNth): Nth  = if (n.isUndefined) undefined else apply(n.intNth)
}
