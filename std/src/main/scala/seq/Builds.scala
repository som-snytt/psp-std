package psp
package std
package core

// import psp.std._

trait Builds[-Elem, +To] {
  def build(xs: Foreach[Elem]): To
}
object Builds {
  implicit def wrap[Elem, To](implicit z: CanBuild[Elem, To]): Builds[Elem, To] = apply(z() ++= _.toTraversable result)
  def apply[Elem, To](f: Foreach[Elem] => To): Builds[Elem, To]                 = new Builds[Elem, To] { def build(xs: Foreach[Elem]): To = f(xs) }
}
