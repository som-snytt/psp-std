package psp

package object std extends psp.std.PackageLevel {
  implicit class JavaIteratorToPsp[A](it: jIterator[A]) {
    def toScalaIterator: sIterator[A] = new ScalaIterator(it)
    def toForeach: Foreach[A]         = each(toScalaIterator)
  }

  implicit class JavaCollectionToPsp[A](xs: jAbstractCollection[A]) {
    def toForeach: Foreach[A]         = each(toTraversable)
    def toTraversable: Traversable[A] = toScalaIterator.toTraversable
    def toScalaIterator: sIterator[A] = new ScalaIterator(xs.iterator)
  }
}
