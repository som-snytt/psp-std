package psp
package std

import api._

object Foreach {
  def builder[A] : Builds[A, Foreach[A]] = Builds(identity)

  final class FromJava[A](xs: jIterable[A]) extends Foreach[A] {
    def size = Size(xs)
    @inline def foreach(f: A => Unit): Unit = BiIterable(xs) foreach f
  }
  final class FromScala[A](xs: sCollection[A]) extends Foreach[A] {
    def size = Size(xs)
    @inline def foreach(f: A => Unit): Unit = xs foreach f
  }
  final class ToScala[A](xs: Foreach[A]) extends sciTraversable[A] {
    def foreach[U](f: A => U): Unit = xs foreach (x => f(x))
  }
  final class Impl[A](val size: Size, mf: Suspended[A]) extends Foreach[A] {
    @inline def foreach(f: A => Unit): Unit = mf(f)
  }
  final case class Joined[A](xs: Foreach[A], ys: Foreach[A]) extends Foreach[A] {
    def size = xs.size + ys.size
    @inline def foreach(f: A => Unit): Unit = {
      xs foreach f
      ys foreach f
    }
  }
  trait AtomicSize[+A] extends Any with Foreach[A] with HasAtomicSize
  trait InfiniteSize[+A] extends Any with AtomicSize[A] {
    def isEmpty  = false
    def size = Infinite
  }
  object KnownSize {
    def unapply[A](xs: Foreach[A]) = xs.size optionally { case x: Atomic => x }
  }

  final case class Constant[A](elem: A) extends InfiniteSize[A] {
    @inline def foreach(f: A => Unit): Unit = while (true) f(elem)
  }
  final case class Continually[A](fn: () => A) extends InfiniteSize[A] {
    @inline def foreach(f: A => Unit): Unit = while (true) f(fn())
  }
  final case class Unfold[A](zero: A)(next: A => A) extends InfiniteSize[A] {
    @inline def foreach(f: A => Unit): Unit = {
      var current = zero
      while (true) {
        f(current)
        current = next(current)
      }
    }
  }

  def elems[A](xs: A*): pSeq[A]                                   = apply[A](xs foreach _)
  def apply[A](mf: Suspended[A]): Foreach[A]                      = new Impl[A](Size.unknown, mf)
  def constant[A](elem: A): Constant[A]                           = Constant[A](elem)
  def continuallySpan[A](p: Predicate[A])(expr: => A): Foreach[A] = continually(expr) takeWhile p
  def continually[A](elem: => A): Continually[A]                  = Continually[A](() => elem)
  def empty[A] : Foreach[A]                                       = Direct.Empty
  def from(n: BigInt): Foreach[BigInt]                            = unfold(n)(_ + 1)
  def from(n: Int): Foreach[Int]                                  = unfold(n)(_ + 1)
  def from(n: Long): Foreach[Long]                                = unfold(n)(_ + 1)
  def join[A](xs: Foreach[A], ys: Foreach[A]): Foreach[A]         = Joined[A](xs, ys)
  def sized[A](size: Size, mf: Suspended[A]): Foreach[A]          = new Impl[A](size, mf)
  def unfold[A](start: A)(next: A => A): Unfold[A]                = Unfold[A](start)(next)
}
