package psp
package std

/** Wrapping java classes.
 */
object NullClassLoader extends jClassLoader
object NullInputStream extends InputStream { def read(): Int = -1 }

object JavaComparator {
  final class Impl[A](f: (A, A) => Int) extends Comparator[A] {
    def compare(x: A, y: A): Int = f(x, y)
  }
  def apply[A](f: (A, A) => Int): Comparator[A] = new Impl[A](f)
}
