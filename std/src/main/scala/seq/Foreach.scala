package psp
package std

import api._

object Foreach {
  def builder[A] : Builds[A, Foreach[A]] = Builds(identity)

  final class ToScala[A](xs: Foreach[A]) extends sciTraversable[A] {
    def foreach[U](f: A => U): Unit = xs foreach (x => f(x))
  }
  final class Impl[A](val sizeInfo: SizeInfo, mf: Suspended[A]) extends Foreach[A] {
    @inline def foreach(f: A => Unit): Unit = mf(f)
  }
  final case class Join[A](xs: Foreach[A], ys: Foreach[A]) extends Foreach[A] {
    def sizeInfo = xs.sizeInfo + ys.sizeInfo
    @inline def foreach(f: A => Unit): Unit = {
      xs foreach f
      ys foreach f
    }
  }
  trait InfiniteForeach[+A] extends Any with Foreach[A] { def sizeInfo = Infinite }

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
  final case class Times[A](size: Precise, elem: A) extends Foreach[A] with HasPreciseSize {
    @inline def foreach(f: A => Unit): Unit = size foreachNth (_ => f(elem))
  }

  def from(n: Int): Foreach[Int]                          = unfold(n)(_ + 1)
  def from(n: Long): Foreach[Long]                        = unfold(n)(_ + 1)
  def from(n: BigInt): Foreach[BigInt]                    = unfold(n)(_ + 1)

  def empty[A] : Foreach[A]                               = Direct.Empty
  def join[A](xs: Foreach[A], ys: Foreach[A]): Foreach[A] = Join[A](xs, ys)
  def constant[A](elem: A): Constant[A]                   = Constant[A](elem)
  def continually[A](elem: => A): Continually[A]          = Continually[A](() => elem)
  def unfold[A](start: A)(next: A => A): Unfold[A]        = Unfold[A](start)(next)
  def times[A](size: Precise, elem: A): Times[A]          = Times[A](size, elem)

  def apply[A](mf: Suspended[A]): Foreach[A] = new Impl[A](SizeInfo.unknown, mf)
  def elems[A](xs: A*): Foreach[A]           = new Impl[A](SizeInfo(xs), xs foreach _)

  def fromScala[A](xs: sCollection[A]): Foreach[A]                          = new Impl[A](SizeInfo(xs), xs.seq foreach _)
  def fromJava[A](xs: jIterable[A]): Foreach[A]                             = new Impl[A](SizeInfo(xs), BiIterable(xs) foreach _)
  def toScala[A, That](xs: Foreach[A])(implicit z: CanBuild[A, That]): That = z() ++= new ToScala(xs) result
  def toJava[A](xs: Foreach[A]): jArrayList[A]                              = new jArrayList[A] doto (b => xs foreach (x => b add x))
}
