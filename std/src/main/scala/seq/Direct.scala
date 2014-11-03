package psp
package std

import api._

object Direct {
  final val Empty: Direct[Nothing] = new Impl[Nothing](0, i => abort(s"Empty($i)"))

  trait DirectImpl[+A] extends Any with api.Direct[A] with api.HasPreciseSize {
    def isEmpty = size.isZero
  }
  trait Forwarder[A] extends Direct[A] {
    protected def underlying: Direct[A]
    def elemAt(i: Index): A         = underlying(i)
    def foreach(f: A => Unit): Unit = underlying foreach f
    def size                        = underlying.size
    def isEmpty                     = underlying.isEmpty
  }

  def build[A](f: Suspended[A]): Direct[A]       = builder[A] direct f
  def builder[A] : Builds[A, Direct[A]]          = arrayBuilder[Any] map wrapArray
  def stringBuilder(): Builds[Char, String]      = arrayBuilder[Char] map (cs => new String(cs))
  def arrayBuilder[A: CTag]: Builds[A, Array[A]] = Builds[A, Array[A]] {
    case xs @ HasSize(n: Precise) => newArray[A](n) doto (arr => xs foreachWithIndex ((x, i) => arr(i.safeInt) = x))
    case xs                       => scala.Array.newBuilder[A] doto (b => xs foreach b.+=) result
  }
  final class ToScala[A](xs: Direct[A]) extends sciIndexedSeq[A] {
    def apply(idx: Int): A = xs elemAt Index(idx)
    def length: Int        = xs.intSize
  }
  abstract class Leaf[+A](val size: Precise, elem: Index => A) extends DirectImpl[A] {
    def elemAt(i: Index): A = elem(i)
    @inline final def foreach(f: A => Unit): Unit = this foreachIndex (i => f(elemAt(i)))
  }
  class TransformIndices[A](xs: Direct[A], f: Index => Index) extends Leaf[A](xs.size, i => xs elemAt f(i))

  final class FromJava[A](xs: jList[A])                            extends Leaf[A](xs.size, xs get _.safeInt)
  final class Impl[A](size: Precise, f: Index => A)                extends Leaf[A](size, f)
  final class WrapString(xs: String)                               extends Leaf[Char](xs.length, xs charAt _.safeInt)
  final class WrapArray[A](xs: Array[_])                           extends Leaf[A](xs.length, i => xs(i.safeInt).castTo[A])
  final case class FromScala[A](scalaCollection: sciIndexedSeq[A]) extends Leaf[A](scalaCollection.length, scalaCollection apply _.safeInt) with psp.std.FromScala[sciIndexedSeq[A]]
  final case class Reversed[A](xs: Direct[A])                      extends TransformIndices(xs, xs.lastIndex - _.safeInt)

  final case class Joined[A](xs: Direct[A], ys: Direct[A]) extends DirectImpl[A] {
    def elemAt(i: Index): A = if (xs containsIndex i) xs elemAt i else ys elemAt (i - xs.intSize)
    def size                = xs.size + ys.size

    @inline final def foreach(f: A => Unit): Unit = andUnit(xs foreach f, ys foreach f)
  }

  def apply[A](xs: A*): Direct[A] = xs match {
    case xs: scmWrappedArray[A] => fromArray(xs.array)
    case xs: sciIndexedSeq[A]   => new FromScala[A](xs)
    case _                      => new Impl[A](xs.size, xs.seq.toVector |> (v => index => v(index.safeInt)))
  }
  def empty[A] : Direct[A]                             = Empty
  def join[A](xs: Direct[A], ys: Direct[A]): Direct[A] = Joined(xs, ys)
  def fromScala[A](xs: sciIndexedSeq[A]): Direct[A]    = new FromScala(xs)
  def fromString(xs: String): Direct[Char]             = new WrapString(xs)
  def fromArray[A](xs: Array[A]): Direct[A]            = new WrapArray[A](xs)
  def wrapArray[A](xs: Array[_]): Direct[A]            = new WrapArray[A](xs)
  def pure[A](size: Precise, f: Index => A): Direct[A] = new Impl(size, f)

  def from(start: Long): DirectView[Long, Direct[Long]] =
    new DirectView(new Impl(Precise(MaxLong), n => start + n.indexValue)) //:| s"Direct.from($start)"
}
