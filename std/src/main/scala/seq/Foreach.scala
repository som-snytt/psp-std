package psp
package std

import SizeInfo._
import scala.collection.immutable

trait Foreach[+A] extends Any with HasSizeInfo { def foreach(f: A => Unit): Unit }

object Foreach {
  def Empty = Direct.Empty

  final class Constant[A](elem: A) extends Foreach[A] {
    def sizeInfo = Infinite
    @inline def foreach(f: A => Unit): Unit = while (true) f(elem)
    override def toString = pp"constant($elem)"
  }

  final case class Unfold[A](zero: A)(next: A => A) extends Foreach[A] {
    def sizeInfo = Infinite
    @inline def foreach(f: A => Unit): Unit = {
      var current = zero
      while (true) {
        f(current)
        current = next(current)
      }
    }
    override def toString = pp"unfold from $zero"
  }

  final case class Times[A](size: Size, elem: A) extends Foreach[A] with HasPreciseSize {
    def sizeInfo = Precise(size)
    @inline def foreach(f: A => Unit): Unit = IntRange.until(0, size.value) foreach (_ => f(elem))
    override def toString = pp"$elem x$size"
  }
  final class PureForeach[+A](mf: Suspended[A], val sizeInfo: SizeInfo) extends Foreach[A] {
    @inline def foreach(f: A => Unit): Unit = mf(f)
    override def toString = pp"$mf"
  }
  final class JoinForeach[+A](xs: Foreach[A], ys: Foreach[A]) extends Foreach[A] {
    def sizeInfo = xs.sizeInfo + ys.sizeInfo
    @inline def foreach(f: A => Unit): Unit = { xs foreach f ; ys foreach f }
    override def toString = pp"$xs ++ $ys"
  }
  final class ToScala[+A](private val xs: Foreach[A]) extends immutable.Traversable[A] {
    def foreach[U](f: A => U): Unit = xs foreach (x => f(x))
  }
  final class FromScala[+A](private val xs: Traversable[A]) extends AnyVal with Foreach[A] {
    def sizeInfo: SizeInfo          = SizeInfo(xs)
    def foreach(f: A => Unit): Unit = xs foreach f
    override def toString           = xs.shortClass + " (wrapped)"
  }

  def from(n: Int): Foreach[Int]       = unfold(n)(_ + 1)
  def from(n: Long): Foreach[Long]     = unfold(n)(_ + 1)
  def from(n: BigInt): Foreach[BigInt] = unfold(n)(_ + 1)

  def const[A](elem: A): Constant[A]            = new Constant(elem)
  def times[A](times: Int, elem: A): Foreach[A] = Times(Size(times), elem)

  def unfold[A](start: A)(next: A => A): Unfold[A]          = Unfold[A](start)(next)
  def traversable[A](xs: GenTraversableOnce[A]): Foreach[A] = new FromScala[A](xs.toTraversable.seq)
  def join[A](xs: Foreach[A], ys: Foreach[A]): Foreach[A]   = new JoinForeach(xs, ys)
  def empty[A] : Foreach[A]                                 = Empty
  def elems[A](xs: A*): Foreach[A]                          = Direct.elems(xs: _*)
  def apply[A](mf: Suspended[A]): Foreach[A]                = new PureForeach[A](mf, unknownSize)

  def stringify[A: Show](xs: Foreach[A], max: Int = 3): String = {
    def prefix = xs.shortClass
    def lp = "("
    def rp = ")"
    def base = pp"""$prefix$lp${xs take max join ", "}"""

    xs.sizeInfo match {
      case Precise(Size(n)) if n <= max => pp"$base$rp"
      case Precise(n)                   => pp"$base, ... $n elements$rp"
      case Infinite                     => pp"$base, ... <inf>$rp"
      case info                         => pp"$base, ... $info$rp"
    }
  }
}
