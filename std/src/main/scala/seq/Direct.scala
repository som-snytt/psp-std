package psp
package std

import api._

object Direct {
  final val Empty: Direct[Nothing] = new Impl[Nothing](newSize(0), i => abort(s"Empty($i)"))

  def builder[A] : Builds[A, Direct[A]] = arrayBuilder[Any] map (res => new WrapArray[A](res))
  def stringBuilder(): Builds[Char, String] = arrayBuilder[Char] map (cs => new String(cs))
  def arrayBuilder[A: CTag]: Builds[A, Array[A]] = Builds[A, Array[A]](xs =>
    xs.sizeInfo match {
      case size: PreciseSize => newArray[A](size) doto (xs foreachCounted _.updated)
      case _                 => psp.std.arrayBuilder[A]() doto (b => xs foreach b.+=) result
    }
  )

  final class ToScala[A](xs: Direct[A]) extends sciIndexedSeq[A] {
    def apply(idx: Int): A = xs elemAt Index(idx)
    def length: Int        = xs.intSize
  }
  abstract class Leaf[+A](val size: PreciseSize) extends Direct[A] with HasPreciseSize {
    @inline final def foreach(f: A => Unit): Unit = this foreachIndex (i => f(elemAt(i)))
    override def toString = if (this.isEmpty) "[ ]" else this.map(_.any_s).optBrackets.to_s
  }
  final class Impl[A](size: PreciseSize, f: Index => A) extends Leaf[A](size) {
    def elemAt(i: Index): A = f(i)
  }
  private class WrapString(xs: String) extends Leaf[Char](newSize(xs.length)) {
    def elemAt(i: Index): Char = xs charAt i.safeToInt
  }
  private class WrapArray[A](xs: Array[_]) extends Leaf[A](newSize(xs.length)) {
    def elemAt(i: Index): A = xs(i.safeToInt).castTo[A]
  }
  private class WrapScalaSeq[A](xs: sciIndexedSeq[A]) extends Leaf[A](newSize(xs.length)) {
    def elemAt(i: Index): A = xs(i.safeToInt)
  }
  final case class Joined[A](xs: Direct[A], ys: Direct[A]) extends Direct[A] {
    def elemAt(i: Index): A = if (xs containsIndex i) xs elemAt i else ys elemAt (i - xs.intSize)
    def size                = xs.size + ys.size
    @inline final def foreach(f: A => Unit): Unit = {
      xs foreach f
      ys foreach f
    }
  }
  class TransformIndices[A](xs: Direct[A], f: Index => Index) extends Leaf[A](xs.size) {
    def elemAt(i: Index): A = xs elemAt f(i)
  }
  final case class Reversed[A](xs: Direct[A]) extends TransformIndices(xs, xs.lastIndex - _.safeToInt)

  def empty[A] : Direct[A]                             = Empty
  def join[A](xs: Direct[A], ys: Direct[A]): Direct[A] = Joined(xs, ys)
  def fromString(xs: String): Direct[Char]             = new WrapString(xs)
  def fromArray[A](xs: Array[A]): Direct[A]            = new WrapArray[A](xs)
  def fromScala[A](xs: sciIndexedSeq[A]): Direct[A]    = new WrapScalaSeq(xs)
  def fill[A](count: Int)(body: => A): Direct[A]       = new WrapArray[A](Array.fill[Any](count)(body))

  def elems[A](xs: A*): Direct[A] = xs match {
    case xs: scmWrappedArray[A] => fromArray(xs.array)
    case xs: sciIndexedSeq[A]   => fromScala(xs)
    case _                      => fromScala(xs.toVector)
  }
}
