package psp
package std

import api._
import Index.zero
import IndexRange.{ undefined, empty }
import lowlevel.ExclusiveIntRange

final class IntIndexRange private[std] (val bits: Long) extends AnyVal with Direct.DirectImpl[Index] with IndexRange with RearSliceable[IndexRange] {
  def startInt      = intRange.start
  def endInt        = intRange.end
  def intRange      = ExclusiveIntRange create bits
  def start         = Index(startInt)
  def end           = Index(endInt)
  def endInclusive  = if (end <= start) NoIndex else end.prev
  def size          = intRange.size
  def precedingSize = newSize(startInt)

  @inline def foreach(f: Index => Unit): Unit = intRange foreach (i => f(Index(i)))

  def elemAt(index: Index): Index = index + intRange.start
  def contains(i: Index): Boolean = intRange contains i.safeToInt

  def >> (n: Int): IndexRange              = IndexRange(intRange >> n)
  def << (n: Int): IndexRange              = IndexRange(intRange << n)
  def drop(n: Precise): IndexRange         = IndexRange(intRange drop n)
  def dropRight(n: Precise): IndexRange    = IndexRange(intRange dropRight n)
  def take(n: Precise): IndexRange         = IndexRange(intRange take n)
  def takeRight(n: Precise): IndexRange    = IndexRange(intRange takeRight n)
  def slice(range: IndexRange): IndexRange = IndexRange(intRange slice range)

  def prefixLength(p: Predicate[Index]): Long    = intRange prefixLength (i => p(Index(i)))
  def dropWhile(p: Predicate[Index]): IndexRange = IndexRange(intRange dropWhile (i => p(Index(i))))
  def takeWhile(p: Predicate[Index]): IndexRange = IndexRange(intRange takeWhile (i => p(Index(i))))

  override def toString = s"[$start,$end)"
}

/** All IndexRanges are inclusive of start and exclusive of end.
 */
object IndexRange {
  def empty: IndexRange                           = new IntIndexRange(0L)
  def undefined: IndexRange                       = new IntIndexRange(-1L)
  def full: IndexRange                            = apply(0, MaxInt)
  def apply(range: ExclusiveIntRange): IndexRange = apply(range.start.zeroPlus, range.end.zeroPlus)
  def apply(start: Int, end: Int): IndexRange     = if (start < 0 || end < 0) undefined else new IntIndexRange(start join64 end)
  def impl(x: IndexRange): IntIndexRange          = new IntIndexRange(x.start.safeToInt join64 x.end.safeToInt)
}
