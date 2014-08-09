package psp
package std

import Index.zero

/** All IndexRanges are inclusive of start and exclusive of end.
 */
final class IndexRange private (private val bits: Long) extends AnyVal {
  private def lbits: Int = (bits >>> 32).toInt
  private def rbits: Int = bits.toInt

  // Shift the range left or right. Attempts to go past the minimum
  // or maximum representable index will yield IndexRange.empty.
  def >> (n: Int): IndexRange = IndexRange.until(start + n, end + n)
  def << (n: Int): IndexRange = IndexRange.until(start - n, end - n)

  def startInt: Int = lbits
  def endInt: Int   = rbits
  def start: Index  = Index(startInt)
  def end: Index    = Index(endInt)
  def length: Long  = (end.toLong - start.toLong) max 0L

  def foreachNth(f: Nth => Unit): Unit            = foreach(i => f(i.toNth))
  def filterNth(p: Nth => Boolean): Vector[Index] = filter(i => p(i.toNth))
  def mapNth[A](f: Nth => A): Vector[A]           = map(i => f(i.toNth))

  def foreachInt(f: Int => Unit): Unit            = toIntRange foreach f
  def filterInt(p: Int => Boolean): Vector[Index] = toIntRange filter p map Index toVector
  def mapInt[A](f: Int => A): Vector[A]           = toIntRange map f toVector

  def filter(p: Index => Boolean): Vector[Index] = filterInt(i => p(Index(i)))
  def foreach(f: Index => Unit): Unit            = foreachInt(i => f(Index(i)))
  def map[A](f: Index => A): Vector[A]           = mapInt(i => f(Index(i)))

  def intersect(that: IndexRange): IndexRange = IndexRange.until(start max that.start, end min that.end)
  def contains(i: Index): Boolean             = i.isDefined && (start <= i && i < end)
  def toSeq: Seq[Index]                       = toVector
  def toVector: Vector[Index]                 = mapInt(Index)
  def toIntRange: Range                       = lbits until rbits

  override def toString = s"[$start,$end)"
}

object IndexRange {
  def empty: IndexRange = new IndexRange(-1L)

  def zeroUntil(end: Index): IndexRange = until(zero, end)
  def zeroTo(end: Index): IndexRange    = to(zero, end)

  def to(start: Index, end: Index): IndexRange =
    if (start.isEmpty || end.isEmpty) empty
    else if (end.value == Int.MaxValue) throw new IllegalArgumentException("Cannot encode Int.MaxValue")
    else until(start, end.next)

  def until(start: Index, end: Index): IndexRange =
    if (start.isEmpty || end.isEmpty) empty
    else if (end < start) until(start, start)
    else new IndexRange((start.value.toLong << 32) | end.value.toLong)
}
