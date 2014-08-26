package psp
package std

trait Builds[-Elem, +To] extends Any {
  def build(xs: Foreach[Elem]): To
}
object Builds {
  def apply[Elem, To](f: Foreach[Elem] => To): BuildsClass[Elem, To] = new BuildsClass(f)

  final class BuildsClass[Elem, To](private val f: Foreach[Elem] => To) extends AnyVal with Builds[Elem, To] {
    def build(xs: Foreach[Elem]): To = f(xs)
  }
}
