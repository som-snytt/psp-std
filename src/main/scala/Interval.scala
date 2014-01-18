package psp
package core

import Interval.Empty

final class Interval private (val start: Index, val end: Index, val isReversed: Boolean) {
  def this(start: Index, end: Index) = this(start, end, isReversed = false)
  require(start >= 0 && end >= 0 && start <= end, ss"$start, $end")

  def reverse: Interval           = new Interval(start, end, !isReversed)
  def drop(n: Int): Interval      = if (n <= 0) this else Interval(start + n, end)
  def dropRight(n: Int): Interval = if (n <= 0) this else Interval(start, end - n)
  def take(n: Int): Interval      = if (n <= 0) Empty else if (intSize <= n) this else Interval(start, start + n)
  def takeRight(n: Int): Interval = if (n <= 0) Empty else if (intSize <= n) this else Interval(end - n, end)
  def contains(index: Index)      = start <= index && index < end

  def slice(s: Int, e: Int): Interval  = if (s < 0) slice(0, e) else if (e <= s) Empty else this drop s take (e - s)
  def slice(range: Interval): Interval = slice(range.start, range.end)

  def isEmpty              = intSize == 0
  def size: Size           = Size(intSize)
  private def intSize: Int = end - start
  def indices: Range       = if (isReversed) (start until end).reverse else start until end
  def firstIndex: Index    = if (isReversed) end - 1 else start
  def lastIndex: Index     = if (isReversed) start else end - 1
  override def toString    = ss"[$firstIndex,$lastIndex]"
}
object Interval {
  val Empty = new Interval(0, 0)
  val Full  = new Interval(0, Int.MaxValue)

  def apply(start: Index, end: Index): Interval =
    if (end <= start || end <= 0) Empty else new Interval(start max 0, end)

  def unapply(x: Interval) = Some((x.start, x.end, x.isReversed))
}
