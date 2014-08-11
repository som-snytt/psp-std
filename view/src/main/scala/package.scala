package psp

import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }
import psp.core.impl._
import psp.std.SizeInfo

package core {
  trait PackageLevel extends PspUtility with PspTypes with PspHighPriority with PspUtilityMethods
}

package object core extends psp.core.PackageLevel with psp.std.PackageLevel {
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
