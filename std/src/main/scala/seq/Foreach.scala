package psp
package std

import api._

object Foreach {
  def builder[A] : Builds[A, Foreach[A]] = Builds(identity)

  final class FromJava[A](xs: jIterable[A]) extends Foreach[A] {
    def sizeInfo = SizeInfo(xs)
    @inline def foreach(f: A => Unit): Unit = BiIterable(xs) foreach f
  }
  final class FromScala[A](xs: sCollection[A]) extends Foreach[A] {
    def sizeInfo = SizeInfo(xs)
    @inline def foreach(f: A => Unit): Unit = xs foreach f
  }
  final class ToScala[A](xs: Foreach[A]) extends sciTraversable[A] {
    def foreach[U](f: A => U): Unit = xs foreach (x => f(x))
  }
  final class Impl[A](val sizeInfo: SizeInfo, mf: Suspended[A]) extends Foreach[A] {
    @inline def foreach(f: A => Unit): Unit = mf(f)
  }
  final case class Joined[A](xs: Foreach[A], ys: Foreach[A]) extends Foreach[A] {
    def sizeInfo = xs.sizeInfo + ys.sizeInfo
    @inline def foreach(f: A => Unit): Unit = {
      xs foreach f
      ys foreach f
    }
  }
  trait ForeachHasSize[+A] extends Any with Foreach[A] with HasKnownSize
  trait InfiniteForeach[+A] extends Any with ForeachHasSize[A] {
    def isEmpty  = false
    def sizeInfo = Infinite
  }
  object KnownSize {
    def unapply[A](xs: Foreach[A]) = xs.sizeInfo optionally { case x: Atomic => x }
  }

  final case class Constant[A](elem: A) extends InfiniteForeach[A] {
    @inline def foreach(f: A => Unit): Unit = while (true) f(elem)
  }
  final case class Continually[A](fn: () => A) extends InfiniteForeach[A] {
    @inline def foreach(f: A => Unit): Unit = while (true) f(fn())
  }
  final case class Unfold[A](zero: A)(next: A => A) extends InfiniteForeach[A] {
    @inline def foreach(f: A => Unit): Unit = {
      var current = zero
      while (true) {
        f(current)
        current = next(current)
      }
    }
  }

  // final case class Times[A](size: PreciseSize, elem: A) extends Foreach[A] with HasPreciseSize {
  //   @inline def foreach(f: A => Unit): Unit = size foreachNth (_ => f(elem))
  // }

  def from(n: Int): Foreach[Int]                          = unfold(n)(_ + 1)
  def from(n: Long): Foreach[Long]                        = unfold(n)(_ + 1)
  def from(n: BigInt): Foreach[BigInt]                    = unfold(n)(_ + 1)

  def empty[A] : Foreach[A]                                                = Direct.Empty
  def join[A](xs: Foreach[A], ys: Foreach[A]): Foreach[A]                  = Joined[A](xs, ys)
  def constant[A](elem: A): Constant[A]                                    = Constant[A](elem)
  def continually[A](elem: => A): Continually[A]                           = Continually[A](() => elem)
  def continuallySpan[A](p: A => Boolean)(expr: => A): Foreach[A]          = continually(expr).m takeWhile p
  def unfold[A](start: A)(next: A => A): Unfold[A]                         = Unfold[A](start)(next)
  // def times[A](size: PreciseSize, elem: A): Foreach[A] with HasPreciseSize = constant(elem) take size.getInt sized size

  def sized[A](sizeInfo: SizeInfo)(mf: Suspended[A]): Foreach[A] = new Impl[A](sizeInfo, mf)
  def apply[A](mf: Suspended[A]): Foreach[A]                     = new Impl[A](SizeInfo.unknown, mf)
}
