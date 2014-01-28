package psp
package core

import scala.{ collection => sc }
import SizeInfo._
import impl._

trait Foreach[+A] extends Any with HasSizeInfo {
  def foreach(f: A => Unit): Unit
}
trait Linear[+A] extends Any with Foreach[A] {
  def isEmpty: Boolean
  def head: A
  def tail: Linear[A]
  def sizeInfo = if (isEmpty) precise(0) else precise(1).atLeast
  @tailrec @inline final def foreach(f: A => Unit): Unit = if (!isEmpty) { f(head) ; tail foreach f }
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
  def Empty = Indexed.Empty

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

  def unfold[A](start: A)(next: A => A): Unfold[A]   = Unfold[A](start)(next)
  def traversable[A](xs: Traversable[A]): Foreach[A] = xs match {
    case xs: sc.IndexedSeq[A] => new ScalaIndexedSeqAsIndexed[A](xs)
    case _                    => new TraversableAsForeach[A](xs)
  }

  def empty[A] : Foreach[A] = Empty
  def apply[A](mf: Suspended[A]): Foreach[A] = new PureForeach[A](mf, SizeInfo.Unknown)
  def elems[A](xs: A*): Foreach[A] = Indexed.elems(xs: _*)

  def stringify[A](xs: Foreach[A], max: Int = 3): String = {
    def prefix = xs.shortClass
    def lp = "("
    def rp = ")"
    def base = pp"""$prefix$lp${xs take max mk_s ", "}"""

    xs.sizeInfo match {
      case Precise(Size(n)) if n <= max => pp"$base$rp"
      case Precise(n)                   => pp"$base, ... $n elements$rp"
      case Infinite                     => pp"$base, ... <inf>$rp"
      case info                         => pp"$base, ... $info$rp"
    }
  }
}

final class ScalaIndexedSeqAsIndexed[+A](underlying: sc.IndexedSeq[A]) extends IndexedImpl[A](Size(underlying.size)) {
  def elemAt(index: Index) = underlying(index)
  override def toString = underlying.shortClass + " (wrapped)"
}

final class TraversableAsForeach[+A](underlying: Traversable[A]) extends Foreach[A] {
  def sizeInfo = underlying match {
    case xs: sc.IndexedSeq[_] => Precise(Size(xs.size))
    case xs                   => if (xs.isEmpty) precise(0) else precise(1).atLeast
  }
  def foreach(f: A => Unit): Unit = underlying foreach f
  override def toString = underlying.shortClass + " (wrapped)"
}

final class ForeachAsTraversable[+A](underlying: Foreach[A]) extends sc.immutable.Traversable[A] {
  def foreach[U](f: A => U): Unit = underlying foreach (x => f(x))
}

final class PureTraversable[+A](mf: Suspended[A]) extends sc.immutable.Traversable[A] {
  def foreach[U](f: A => U): Unit = mf(x => f(x))
}

trait ForeachImplicits {
  implicit def tuple2ToForeach[A](p: Tuple2[A, A]): Foreach[A]          = Foreach.elems(p._1, p._2)
  implicit def tuple3ToForeach[A](p: Tuple3[A, A, A]): Foreach[A]       = Foreach.elems(p._1, p._2, p._3)
  implicit def tuple4ToForeach[A](p: Tuple4[A, A, A, A]): Foreach[A]    = Foreach.elems(p._1, p._2, p._3, p._4)
  implicit def tuple5ToForeach[A](p: Tuple5[A, A, A, A, A]): Foreach[A] = Foreach.elems(p._1, p._2, p._3, p._4, p._5)

  implicit def implicitForeachOps[A](xs: Foreach[A]): ForeachOperations[A]          = new ForeachOperations(xs)
  implicit def implicitForeachConversions[A](xs: Foreach[A]): ForeachConversions[A] = new ForeachConversions(xs)
}
