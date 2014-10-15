package psp
package std

import api._
import Index.zero
import IndexRange.{ undefined, empty }

/** All IndexRanges are inclusive of start and exclusive of end.
 */
final class IndexRange private (val bits: Long) extends AnyVal {
  private def lbits: Int = (bits >>> 32).toInt
  private def rbits: Int = bits.toInt

  // Shift the range left or right. Attempts to go past the minimum
  // or maximum representable index will yield IndexRange.undefined.
  def >> (n: Int): IndexRange = IndexRange.until(start + n, end + n)
  def << (n: Int): IndexRange = IndexRange.until(start - n, end - n)

  def isUndefined = this == undefined
  def isDefined   = !isUndefined

  def slice(range: IndexRange): IndexRange =
    if (this.isUndefined || range.isUndefined) undefined
    else this drop range.start.value take range.intLength

  def startInt: Int  = lbits
  def endInt: Int    = rbits
  def start: Index   = Index(startInt)
  def end: Index     = Index(endInt)
  def length: Long   = (end.toLong - start.toLong) max 0L
  def intLength: Int = length.toInt
  def size: Size     = if (length > MaxInt) NoSize else Size(intLength)

  def drop(n: Int): IndexRange                   = if (isUndefined) undefined else if (n <= 0) this else IndexRange.until(start + n, end)
  def dropRight(n: Int): IndexRange              = if (isUndefined) undefined else if (n <= 0) this else IndexRange.until(start, end - n)
  def take(n: Int): IndexRange                   = if (isUndefined) undefined else if (n <= 0) empty else if (intLength <= n) this else IndexRange.until(start, start + n)
  def takeRight(n: Int): IndexRange              = if (isUndefined) undefined else if (n <= 0) empty else if (intLength <= n) this else IndexRange.until(end - n, end)
  def dropWhile(p: Index => Boolean): IndexRange = find(p).fold(this)(drop)
  def takeWhile(p: Index => Boolean): IndexRange = find(!p).fold(empty)(take)

  def foreachNth(f: Nth => Unit): Unit            = foreach(i => f(i.toNth))
  def filterNth(p: Nth => Boolean): Vector[Index] = filter(i => p(i.toNth))
  def mapNth[A](f: Nth => A): Vector[A]           = map(i => f(i.toNth))

  def foreachInt(f: Int => Unit): Unit                  = toIntRange foreach f
  def filterInt(p: Int => Boolean): Vector[Index]       = toIntRange filter p map Index toVector
  def mapInt[A](f: Int => A): Vector[A]                 = toIntRange map f toVector
  def findInt[A](p: Int => Boolean): Option[Int]        = toIntRange find p
  def findIntReverse[A](p: Int => Boolean): Option[Int] = toIntRange.reverse find p

  def filter(p: Index => Boolean): Vector[Index] = filterInt(i => p(Index(i)))
  def foreach(f: Index => Unit): Unit            = foreachInt(i => f(Index(i)))
  def map[A](f: Index => A): Vector[A]           = mapInt(i => f(Index(i)))
  def find(p: Index => Boolean): Index           = findInt(i => p(Index(i))).fold(NoIndex)(Index)
  def findReverse(p: Index => Boolean): Index    = findIntReverse(i => p(Index(i))).fold(NoIndex)(Index)
  def exists(p: Index => Boolean): Boolean       = find(p).isDefined
  def forall(p: Index => Boolean): Boolean       = !exists(x => !p(x))

  def intersect(that: IndexRange): IndexRange = IndexRange.until(start max that.start, end min that.end)
  def contains(i: Index): Boolean             = !i.isUndefined && (start <= i && i < end)
  def toSeq: Seq[Index]                       = toVector
  def toVector: Vector[Index]                 = mapInt(Index)
  def toIntRange: sciRange                    = sciRange(lbits, rbits)
  def toScalaRange: sciRange                  = sciRange(lbits, rbits)

  override def toString = s"[$start,$end)"
}

object IndexRange {
  implicit def ShowIndexRange = Show.native[IndexRange]()

  def full: IndexRange                  = until(zero, Index.max)
  def empty: IndexRange                 = until(zero, zero)
  def undefined: IndexRange             = new IndexRange(-1L)
  def zeroUntil(end: Index): IndexRange = until(zero, end)
  def zeroTo(end: Index): IndexRange    = to(zero, end)

  def to(start: Index, end: Index): IndexRange =
    if (start.isUndefined || end.isUndefined) undefined
    else if (end.value == MaxInt) throw new IllegalArgumentException("Cannot create IndexRange which contains Int.MaxValue")
    else until(start, end.next)

  def until(start: Index, end: Index): IndexRange =
    if (start.isUndefined || end.isUndefined) undefined
    else if (end < start) until(start, start)
    else new IndexRange((start.toLong << 32) | end.toLong)
}
