package psp
package std

import api._

object Direct {
  def builder[A] : Builds[A, Direct[A]] = arrayBuilder[Any] map (res => new WrapArray[A](res))
  def stringBuilder(): Builds[Char, String] = arrayBuilder[Char] map (cs => new String(cs))
  def arrayBuilder[A: CTag]: Builds[A, Array[A]] = Builds[A, Array[A]](xs =>
    xs.sizeInfo match {
      case Precise(size) => new Array[A](size) doto (arr => xs foreachCounted arr.updated)
      case _             => scala.Array.newBuilder[A] doto (buf => xs foreach (x => buf += x)) result
    }
  )

  final class ToScala[A](xs: Direct[A]) extends sciIndexedSeq[A] {
    def apply(idx: Int): A = xs elemAt idx.index
    def length: Int        = xs.size.sizeValue
  }
  abstract class Leaf[+A](val size: Precise) extends Direct[A] with HasPreciseSize {
    @inline final def foreach(f: A => Unit): Unit = size foreachIndex (i => f(elemAt(i)))
    // override def toString = if (this.isEmpty) "[ ]" else this.map(_.any_s).optBrackets.to_s
  }
  final class Impl[A](size: Precise, f: Index => A) extends Leaf[A](size) {
    def elemAt(i: Index): A = f(i)
  }
  private class WrapString(xs: String) extends Leaf[Char](xs.length.size) {
    def elemAt(i: Index): Char = xs charAt i.indexValue
  }
  private class WrapArray[A](xs: Array[_]) extends Leaf[A](xs.length.size) {
    def elemAt(i: Index): A = xs(i.indexValue).castTo[A]
  }
  private class WrapScalaSeq[A](xs: sciIndexedSeq[A]) extends Leaf[A](xs.length.size) {
    def elemAt(i: Index): A = xs(i.indexValue)
  }
  final case class Joined[A](xs: Direct[A], ys: Direct[A]) extends Direct[A] {
    def elemAt(i: Index): A = if (xs containsIndex i) xs elemAt i else ys elemAt (i - xs.size.get)
    def size                = xs.size + ys.size
    @inline final def foreach(f: A => Unit): Unit = {
      xs foreach f
      ys foreach f
    }
  }
  final case class Reversed[A](xs: Direct[A]) extends Leaf[A](xs.size) {
    def elemAt(i: Index): A = xs elemAt (size.lastIndex - i.indexValue)
  }
  final case object Empty extends Leaf[Nothing](0.size) {
    def elemAt(i: Index): Nothing = abort(s"Empty($i)")
  }

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
