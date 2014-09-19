package psp
package std

trait Builds[-Elem, +To] extends Any {
  def build(xs: Foreach[Elem]): To
}
object Builds {
  def apply[Elem, To](f: Foreach[Elem] => To): BuildsClass[Elem, To] = new BuildsClass(f)
  def wrap[Elem, To](z: CanBuild[Elem, To]): Builds[Elem, To]        = apply[Elem, To](xs => z() ++= new Foreach.ToScala(xs) result)

  final class BuildsClass[Elem, To](val f: Foreach[Elem] => To) extends AnyVal with Builds[Elem, To] {
    def build(xs: Foreach[Elem]): To = f(xs)
  }
}
