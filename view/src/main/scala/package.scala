package psp

import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }
import psp.core.impl._
import psp.std.{ IndexRange, SizeInfo }

package core {
  trait PackageLevel extends PspUtility with PspTypes with PspHighPriority with PspUtilityMethods
}

package object core extends psp.core.PackageLevel with psp.std.PackageLevel {
  // Temporary helpers to ease reconciliation with psp.std.
  type Index   = psp.std.Index
  type Size = psp.std.Size
  val Size = psp.std.Size

  def intRange(start: Int, end: Int): Direct[Int] = IntRange.until(start, end)

  implicit class TemporarySizeOps(val s: Size) {
    def toIndexed: Direct[Int]       = intRange(0, s.value)
    def reverseInterval: Direct[Int] = toIndexed.reverse
  }
  implicit class TemporaryIndexRangeOps(val r: IndexRange) {
    def toIndexed: Direct[Int] = intRange(r.startInt, r.endInt)
  }

  implicit def intToIndex(x: Int): psp.std.Index = psp.std.Index(x)
  implicit def indexToInt(x: psp.std.Index): Int = x.intIndex

  def unknownSize = SizeInfo.Unknown

  implicit class TraversableToPsp[A](xs: GenTraversableOnce[A]) {
    def toPsp: Foreach[A] = Foreach traversable xs
  }

  implicit class JavaIteratorToPsp[A](xs: jIterator[A]) {
    def toScalaIterator = new scala.Iterator[A] {
      def next    = xs.next
      def hasNext = xs.hasNext
    }
    def toPsp: Foreach[A] = toScalaIterator.toTraversable.toPsp
  }

  implicit class JavaCollectionToPsp[A](xs: jAbstractCollection[A]) {
    def toPsp: Foreach[A] = toTraversable.toPsp
    def toTraversable: Traversable[A] = toScalaIterator.toTraversable
    def toScalaIterator = new scala.Iterator[A] {
      val it = xs.iterator
      def next    = it.next
      def hasNext = it.hasNext
    }
  }
}
