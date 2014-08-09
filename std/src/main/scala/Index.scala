package psp
package std

/** A valid index is always non-negative. All negative indices are
 *  mapped to NoIndex, which has an underlying value of -1.
 *  In principle we could double the usable range by treating
 *  value as an unsigned Int other than -1.
 */
final class Index private (val value: Int) extends AnyVal with Ordered[Index] {
  def +(n: Int): Index = Index(value + n)
  def -(n: Int): Index = Index(value - n)

  def until(end: Index): IndexRange = IndexRange.until(value, end.value)
  def to(end: Index): IndexRange    = IndexRange.to(value, end.value)

  def toZero       = value to 0 by -1 map Index
  def indicesUntil = Index(0) until this
  def indicesTo    = Index(0) to this

  def isDefined   = this != Index.empty
  def isUndefined = this == Index.empty

  def spaces                    = " " * value
  def compare(that: Index): Int = value compare that.value
  def prev                      = Index(value - 1)
  def next                      = Index(value + 1)

  def max(that: Index): Index = Index(value max that.value)
  def min(that: Index): Index = Index(value min that.value)

  def toInt: Int   = value
  def toLong: Long = value.toLong

  override def toString         = s"$value"
}
object Index extends (Int => Index) {
  def empty: Index = new Index(-1)
  def apply(value: Int): Index = if (value < 0) empty else new Index(value)
}
