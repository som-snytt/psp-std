package psp
package core

import scala.reflect.io.Streamable

final class UniversalOps[T](val x: T) extends AnyVal {
  def nTimes(n: Int): Foreach[T] = Foreach.times(n, x)
  def ref_==(y: Any): Boolean    = x.toRef eq y.toRef

  def toElements: Foreach[Any] = x match {
    case xs: Foreach[_]     => xs
    case xs: Traversable[_] => Foreach[Any](xs foreach _)
    case _                  => Foreach elems x
  }
  def trunc_s(maxlen: Int): String = x.try_s match {
    case s if s.length <= maxlen => s
    case s                       => pp"${s.substring(0, (maxlen - 3) max 0)}..."
  }
  def shortClass: String = decodeName(x.getClass.getName split "[.]" last)
}

// final class JavaPathOps(val path: jPath) extends AnyVal {
//   def toUrl = path.toUri.toURL
//   def slurp(): String = Streamable slurp toUrl
// }
