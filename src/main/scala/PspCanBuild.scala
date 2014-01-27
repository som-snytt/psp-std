package psp
package core

trait PspCanBuild[-Elem, +To] {
  def build(xs: Foreach[Elem]): To
}

final class PspCanBuildImpl[-Elem, +To](f: Foreach[Elem] => To) extends PspCanBuild[Elem, To] {
  def build(xs: Foreach[Elem]): To = f(xs)
}

class CanBuildFromWrapper[-Elem, +To](cbf: CanBuildFrom[Nothing, Elem, To]) extends PspCanBuild[Elem, To] {
  def build(xs: Foreach[Elem]): To = (cbf() ++= xs.toTraversable).result
}

object PspCanBuild {
  def wrap[Elem, To](cbf: CanBuildFrom[_, Elem, To]): PspCanBuild[Elem, To] = new CanBuildFromWrapper(cbf)
  def apply[Elem, To](f: Foreach[Elem] => To): PspCanBuild[Elem, To]        = new PspCanBuildImpl(f)

  implicit def translateCanBuildFrom[Elem, To](implicit cbf: CanBuildFrom[Nothing, Elem, To]): PspCanBuild[Elem, To] = wrap(cbf)
}
