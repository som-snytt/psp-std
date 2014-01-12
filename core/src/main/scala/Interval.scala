package psp
package core

final class Interval private (val start: Index, val end: Index) {
  require(0 <= start && 0 <= end, s"$start, $end")

  def drop(n: Int): Interval = if (n <= 0) this else Interval(start + n min end, end)
  def take(n: Int): Interval = if (n <= 0) Interval.Empty else Interval(start, start + n min end)

  def size: Size        = Size(end - start)
  def indices: Range    = start until end
  def lastIndex: Index  = end - 1
  override def toString = ss"[$start,$end)"
}
object Interval {
  val Empty = new Interval(0, 0)

  def apply(start: Index, end: Index): Interval =
    if (end <= start || end <= 0) Empty else new Interval(start max 0, end)

  def unapply(x: Interval) = Some(x.start -> x.end)
}
