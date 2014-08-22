package psp

package object std extends psp.std.PackageLevel {
  final implicit class ScalaIterator[A](xs: jIterator[A]) extends scala.Iterator[A] {
    def next    = xs.next
    def hasNext = xs.hasNext
  }
}
