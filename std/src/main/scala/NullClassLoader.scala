package psp
package std

/** Wrapping java classes.
 */
object NullClassLoader extends ClassLoader
object NullInputStream extends InputStream { def read(): Int = -1 }

final class ScalaIterator[A](xs: jIterator[A]) extends scala.Iterator[A] {
  def next    = xs.next
  def hasNext = xs.hasNext
}
