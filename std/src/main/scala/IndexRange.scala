package psp
package std

import api._
import Index.zero
import IndexRange.{ undefined, empty }
import lowlevel.ExclusiveIntRange

final class IntIndexRange private[std] (val bits: Long) extends AnyVal with IndexRange {
  def intRange      = ExclusiveIntRange create bits
  def start         = intRange.start.index
  def end           = intRange.end.index
  def endInclusive  = if (end <= start) NoIndex else end.prev
  def size: Precise = intRange.size

  @inline def foreach(f: Index => Unit): Unit = intRange foreach (i => f(i.index))

  def elemAt(index: Index): Index = index + intRange.start
  def contains(i: Index): Boolean = !i.isEmpty && (intRange contains i.indexValue)

  def >> (n: Int): IndexRange              = IndexRange(intRange >> n)
  def << (n: Int): IndexRange              = IndexRange(intRange << n)
  def drop(n: Int): IndexRange             = IndexRange(intRange drop n)
  def dropRight(n: Int): IndexRange        = IndexRange(intRange dropRight n)
  def take(n: Int): IndexRange             = IndexRange(intRange take n)
  def takeRight(n: Int): IndexRange        = IndexRange(intRange takeRight n)
  def slice(range: IndexRange): IndexRange = IndexRange(intRange slice range)
  def limitTo(x: Size): IndexRange         = start until (x min size).sizeValue.index //IndexRange(start.indexValue, x.sizeValue min size.sizeValue)
  def extendBy(x: Size): IndexRange        = IndexRange(start.indexValue, (size + x).sizeValue)

  def prefixLength(p: Index => Boolean): Int     = intRange prefixLength ((i: Int) => p(i.index))
  def dropWhile(p: Index => Boolean): IndexRange = IndexRange(intRange dropWhile (i => p(i.index)))
  def takeWhile(p: Index => Boolean): IndexRange = IndexRange(intRange takeWhile (i => p(i.index)))

  override def toString = s"[$start,$end)"
}

/** All IndexRanges are inclusive of start and exclusive of end.
 */
object IndexRange {
  def empty: IndexRange                           = new IntIndexRange(0L)
  def undefined: IndexRange                       = new IntIndexRange(-1L)
  def full: IndexRange                            = apply(0, MaxInt)
  def apply(range: ExclusiveIntRange): IndexRange = apply(range.start max 0, range.end max 0)
  def apply(start: Int, end: Int): IndexRange     = if (start < 0 || end < 0) undefined else new IntIndexRange(start join end)
  def impl(x: IndexRange): IntIndexRange          = new IntIndexRange(x.start.indexValue join x.end.indexValue)
}
