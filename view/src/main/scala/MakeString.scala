package psp
package core

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
