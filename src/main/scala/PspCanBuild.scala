package psp
package core

trait PspCanBuild[-Elem, +To] {
  def build(xs: Foreach[Elem]): To
}

object PspCanBuild {
  def wrap[Elem, To](cbf: CanBuildFrom[_, Elem, To]): PspCanBuild[Elem, To] = apply(_ buildInto cbf)
  def apply[Elem, To](f: Foreach[Elem] => To): PspCanBuild[Elem, To]        = new PspCanBuild[Elem, To] { def build(xs: Foreach[Elem]): To = f(xs) }

  implicit def raiseCanBuildFrom[Elem, To](implicit cbf: CanBuildFrom[Nothing, Elem, To]): PspCanBuild[Elem, To] = wrap(cbf)
}
