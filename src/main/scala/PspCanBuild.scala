package psp
package core

trait Builds[-Elem, +To] {
  def build(xs: Foreach[Elem]): To
}

object Builds {
  def wrap[Elem, To](cbf: CanBuildFrom[Nothing, Elem, To]): Builds[Elem, To] = apply(_ buildInto cbf)
  def apply[Elem, To](f: Foreach[Elem] => To): Builds[Elem, To]              = new Builds[Elem, To] { def build(xs: Foreach[Elem]): To = f(xs) }

  implicit def raiseCanBuildFrom[Elem, To](implicit cbf: CanBuildFrom[Nothing, Elem, To]): Builds[Elem, To] = wrap(cbf)
}
