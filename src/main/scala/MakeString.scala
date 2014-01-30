package psp
package core

trait ToString extends Any {
  def to_s: String
}

final class UniversalOps[T](val x: T) extends AnyVal with ToString {
  def castTo[U] : U              = x.asInstanceOf[U]
  def nTimes(n: Int): Foreach[T] = Foreach.times(n, x)
  def toRef: AnyRef              = castTo[AnyRef]
  def ref_==(y: Any): Boolean    = x.toRef eq y.toRef

  def toElements: Foreach[Any] = x match {
    case xs: Foreach[_]     => xs
    case xs: Traversable[_] => Foreach[Any](xs foreach _)
    case _                  => Foreach elems x
  }

  def trunc_s(maxlen: Int): String = to_s match {
    case s if s.length <= maxlen => s
    case s                       => pp"${s.substring(0, (maxlen - 3) max 0)}..."
  }
  def to_s: String = x match {
    case x: ToString              => x.to_s
    case x: Labeled               => x.label
    case _: PartialFunction[_, _] => "<pf>"
    case _: Function1[_, _]       => "<f>"
    case _                        => "" + x
  }
  def pp: String = x match {
    case xs: Foreach[_] => "%15s  %s".format(xs.sizeInfo.to_s, xs.to_s)
    case _              => x.to_s
  }
  def shortClass: String = decodeName(x.getClass.getName split "[.]" last)
}
