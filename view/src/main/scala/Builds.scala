package psp
package core

import psp.std._

trait Builds[-Elem, +To] {
  def build(xs: Foreach[Elem]): To
}
object Builds {
  def wrap[Elem, To](cbf: CanBuild[Elem, To]): Builds[Elem, To] = apply(_ buildInto cbf)
  def apply[Elem, To](f: Foreach[Elem] => To): Builds[Elem, To] = new Builds[Elem, To] { def build(xs: Foreach[Elem]): To = f(xs) }
}
