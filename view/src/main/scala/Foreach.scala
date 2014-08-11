package psp
package core

import scala.{ collection => sc }
import SizeInfo._
import impl._

trait Foreach[+A] extends Any with HasSizeInfo {
  def foreach(f: A => Unit): Unit
}
trait Linear[+A] extends Any with Foreach[A] {
  type Tail <: Linear[A]

  def isEmpty: Boolean
  def head: A
  def tail: Tail
}
trait LinearImpl[+A] extends Any with Linear[A] {
  def sizeInfo = if (isEmpty) Empty else NonEmpty
  @inline final def foreach(f: A => Unit): Unit = {
    @tailrec def loop(xs: Linear[A]): Unit = if (!xs.isEmpty) { f(xs.head) ; loop(xs.tail) }
    loop(this)
  }
}
trait InvariantLinear[A] extends Any with Linear[A] with Invariant[A]

object InvariantLinear {
  implicit final class InvariantLinearOps[A](val xs: InvariantLinear[A]) extends AnyVal {
    def contains(x: A): Boolean = {
      @tailrec def loop(xs: Linear[A]): Boolean = !xs.isEmpty && (x == xs.head || loop(xs.tail))
      loop(xs)
    }
  }
}

object Foreach extends ForeachImplicits {
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

  final case class Times[A](size: Size, elem: A) extends Foreach[A] with HasPreciseSizeImpl {
    @inline def foreach(f: A => Unit): Unit = 0 until size.value foreach (_ => f(elem))
    override def toString = pp"$elem x$size"
  }

  final class PureForeach[+A](mf: Suspended[A], val sizeInfo: SizeInfo) extends Foreach[A] {
    @inline def foreach(f: A => Unit): Unit = mf(f)
    override def toString = pp"$mf"
  }

  def from(n: Int): Foreach[Int]       = unfold(n)(_ + 1)
  def from(n: Long): Foreach[Long]     = unfold(n)(_ + 1)
  def from(n: BigInt): Foreach[BigInt] = unfold(n)(_ + 1)

  def to(start: Int, last: Int): Foreach[Int]     = PspList fromForeach IntRange.to(start, last)
  def const[A](elem: A): Constant[A]              = new Constant(elem)
  def times[A](times: Int, elem: A): Foreach[A]   = Times(Size(times), elem)

  def unfold[A](start: A)(next: A => A): Unfold[A]          = Unfold[A](start)(next)
  def traversable[A](xs: GenTraversableOnce[A]): Foreach[A] = FromScala(xs)

  def join[A](xs: Foreach[A], ys: Foreach[A]): Foreach[A] = {
    val sizeInfo = xs.sizeInfo + ys.sizeInfo
    val mf: Suspended[A] = f => { xs foreach f ; ys foreach f }
    new PureForeach(mf, sizeInfo)
  }
  def empty[A] : Foreach[A] = Empty
  def apply[A](mf: Suspended[A]): Foreach[A] = new PureForeach[A](mf, unknownSize)
  def elems[A](xs: A*): Foreach[A] = Direct.elems(xs: _*)

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

trait ForeachImplicits {
  implicit def tuple2ToForeach[A](p: Tuple2[A, A]): Foreach[A]          = Foreach.elems(p._1, p._2)
  implicit def tuple3ToForeach[A](p: Tuple3[A, A, A]): Foreach[A]       = Foreach.elems(p._1, p._2, p._3)
  implicit def tuple4ToForeach[A](p: Tuple4[A, A, A, A]): Foreach[A]    = Foreach.elems(p._1, p._2, p._3, p._4)
  implicit def tuple5ToForeach[A](p: Tuple5[A, A, A, A, A]): Foreach[A] = Foreach.elems(p._1, p._2, p._3, p._4, p._5)

  implicit def implicitForeachOps[A](xs: Foreach[A]): ForeachOperations[A] = new ForeachOperations(xs)
}
