package psp
package std

import api._
// , StdShow._

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
  final class FromJava[A](xs: jList[A]) extends Leaf[A](newSize(xs.size)) {
    def elemAt(i: Index): A = xs get i.safeToInt
  }
  final class FromScala[A](xs: sciIndexedSeq[A]) extends Leaf[A](newSize(xs.length)) {
    def elemAt(i: Index): A = xs(i.safeToInt)
  }
  final class ToScala[A](xs: Direct[A]) extends sciIndexedSeq[A] {
    def apply(idx: Int): A = xs elemAt Index(idx)
    def length: Int        = xs.intSize
  }
  abstract class Leaf[+A](val size: PreciseSize) extends Direct[A] with HasPreciseSize {
    @inline final def foreach(f: A => Unit): Unit = this foreachIndex (i => f(elemAt(i)))
    // override def toString = if (this.isEmpty) "[ ]" else "$this"
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

  def apply[A](xs: A*): Direct[A] = xs match {
    case xs: scmWrappedArray[A] => fromArray(xs.array)
    case xs: sciIndexedSeq[A]   => new FromScala[A](xs)
    case _                      => new Impl[A](newSize(xs.size), xs.seq.toVector |> (v => index => v(index.safeToInt)))
  }
  def empty[A] : Direct[A]                             = Empty
  def join[A](xs: Direct[A], ys: Direct[A]): Direct[A] = Joined(xs, ys)
  def fromString(xs: String): Direct[Char]             = new WrapString(xs)
  def fromArray[A](xs: Array[A]): Direct[A]            = new WrapArray[A](xs)
  def fill[A](count: Int)(body: => A): Direct[A]       = new WrapArray[A](Array.fill[Any](count)(body))
}
