package psp
package core

import psp.std._

trait Builds[-Elem, +To] {
  def +=(elem: Elem): this.type
  def ++=(xs: TraversableOnce[Elem]): this.type
  def build(xs: Foreach[Elem]): To
  def build(xss: GenTraversableOnce[Elem]*): To
  def newBuilder(): Builder[Elem, To]
}

class CompatBuilder[-Elem, +To](f: Foreach[Elem] => To) extends Builder[Elem, To] with Builds[Elem, To] {
  private[this] def bufferToBuilder(buffer: ArrayBuffer[Elem]): Builder[Elem, To] = buffer mapResult (xs => f(Foreach traversable xs))
  private[this] val buf = new ArrayBuffer[Elem]

  def +=(elem: Elem): this.type = try this finally buf += elem
  def clear(): Unit = buf.clear()
  def result(): To = bufferToBuilder(buf).result
  def build(xs: Foreach[Elem]): To = f(xs)
  def build(xss: GenTraversableOnce[Elem]*): To = f(Foreach(f => xss foreach (xs => xs foreach f)))
  def newBuilder(): Builder[Elem, To] = bufferToBuilder(new ArrayBuffer[Elem]())
}

object Builds {
  def wrap[Elem, To](cbf: CanBuildFrom[Nothing, Elem, To]): Builds[Elem, To] = apply(_ buildInto cbf)
  def apply[Elem, To](f: Foreach[Elem] => To): Builds[Elem, To]              = new CompatBuilder[Elem, To](f)

  // implicit def raiseCanBuildFrom[Elem, To](implicit cbf: CanBuildFrom[Nothing, Elem, To]): Builds[Elem, To] = wrap(cbf)
}
