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
  def builder[A] : Builds[A, Direct[A]]          = arrayBuilder[Any] map (res => new WrapArray[A](res))
  def stringBuilder(): Builds[Char, String]      = arrayBuilder[Char] map (cs => new String(cs))
  def arrayBuilder[A: CTag]: Builds[A, Array[A]] = Builds[A, Array[A]](xs =>
    xs.size match {
      case size: Precise => newArray[A](size) doto (arr => xs foreachWithIndex ((x, i) => arr(i.safeToInt) = x))
      case _             => scala.Array.newBuilder[A] doto (b => xs foreach b.+=) result
    }
  )
  final class FromJava[A](xs: jList[A]) extends Leaf[A](xs.size) {
    def elemAt(i: Index): A = xs get i.safeToInt
  }
  final case class FromScala[A](scalaCollection: sciIndexedSeq[A]) extends Leaf[A](scalaCollection.length) with psp.std.FromScala[sciIndexedSeq[A]] {
    def elemAt(i: Index): A = scalaCollection(i.safeToInt)
  }
  final class ToScala[A](xs: Direct[A]) extends sciIndexedSeq[A] {
    def apply(idx: Int): A = xs elemAt Index(idx)
    def length: Int        = xs.intSize
  }
  abstract class Leaf[+A](val size: Precise) extends DirectImpl[A] {
    @inline final def foreach(f: A => Unit): Unit = this foreachIndex (i => f(elemAt(i)))
  }
  final class Impl[A](size: Precise, f: Index => A) extends Leaf[A](size) {
    def elemAt(i: Index): A = f(i)
  }
  private class WrapString(xs: String) extends Leaf[Char](xs.length) {
    def elemAt(i: Index): Char = xs charAt i.safeToInt
  }
  final class WrapArray[A](xs: Array[_]) extends Leaf[A](xs.length) {
    def elemAt(i: Index): A = xs(i.safeToInt).castTo[A]
  }
  final case class Joined[A](xs: Direct[A], ys: Direct[A]) extends DirectImpl[A] {
    def elemAt(i: Index): A = if (xs containsIndex i) xs elemAt i else ys elemAt (i - xs.intSize)
    def size            = xs.size + ys.size
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
    case _                      => new Impl[A](xs.size, xs.seq.toVector |> (v => index => v(index.safeToInt)))
  }
  def empty[A] : Direct[A]                             = Empty
  def join[A](xs: Direct[A], ys: Direct[A]): Direct[A] = Joined(xs, ys)
  def fromScala[A](xs: sciIndexedSeq[A]): Direct[A]    = new FromScala(xs)
  def fromString(xs: String): Direct[Char]             = new WrapString(xs)
  def fromArray[A](xs: Array[A]): Direct[A]            = new WrapArray[A](xs)
  def pure[A](size: Precise, f: Index => A): Direct[A] = new Impl(size, f)

  def from(start: Long): DirectView[Long, Direct[Long]] =
    new DirectView(new Impl(Precise(MaxLong), n => start + n.indexValue)) //:| s"Direct.from($start)"

  def unapplySeq[A](xs: Direct[A]): scala.Some[sciIndexedSeq[A]] = Some(new ToScala(xs))
}
