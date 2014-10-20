package psp
package std

import api._, StdShow._

final case class FunctionGrid[A, B](values: pVector[A], functions: pVector[A => B]) {
  def rows: pVector[pVector[B]]                         = values map (v => functions map (f => f(v)))
  def columns: pVector[pVector[B]]                      = functions map (f => values map (v => f(v)))
  def renderLines(implicit z: Show[B]): pVector[String]               = {
    val widths = columns map (col => col map (x => (z show x).length) max)
    val rowFormat = widths map (_.size.leftFormatString) mkString " "
    rows map (row => rowFormat.format(row.seq: _*))
  }
  def render(implicit z: Show[B]): String = renderLines.joinLines.render
}

object Direct {
  final val Empty: Direct[Nothing] = new Impl[Nothing](newSize(0), i => abort(s"Empty($i)"))

  trait DirectImpl[+A] extends Any with api.Direct[A] with api.HasPreciseSize {
    def isEmpty = sizeInfo.value == 0L
  }
  trait Forwarder[A] extends Direct[A] {
    protected def underlying: Direct[A]

    def elemAt(i: Index): A         = underlying(i)
    def foreach(f: A => Unit): Unit = underlying foreach f
    def sizeInfo                    = underlying.sizeInfo
    def isEmpty                     = underlying.isEmpty
  }

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
  abstract class Leaf[+A](val sizeInfo: PreciseSize) extends DirectImpl[A] {
    @inline final def foreach(f: A => Unit): Unit = this foreachIndex (i => f(elemAt(i)))
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
  final case class Joined[A](xs: Direct[A], ys: Direct[A]) extends DirectImpl[A] {
    def elemAt(i: Index): A = if (xs containsIndex i) xs elemAt i else ys elemAt (i - xs.intSize)
    def sizeInfo            = xs.sizeInfo + ys.sizeInfo
    @inline final def foreach(f: A => Unit): Unit = {
      xs foreach f
      ys foreach f
    }
  }
  class TransformIndices[A](xs: Direct[A], f: Index => Index) extends Leaf[A](xs.sizeInfo) {
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

  def unapplySeq[A](xs: Direct[A]): Some[sciIndexedSeq[A]] = Some(new ToScala(xs))
}
