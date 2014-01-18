package psp
package core

import psp.common.Labeled
import scala.{ collection => sc }
import SizeInfo._

final case class SizedForeach[+A](xs: Foreach[A], size: Size) extends Foreach[A] with HasPreciseSize {
  @inline final def foreach(f: A => Unit): Unit = xs foreach f
  override def toString = s"$xs (size=$size)"
}

trait Foreach[+A] extends Any with HasSizeInfo {
  def foreach(f: A => Unit): Unit
}
trait Linear[+A] extends Any with Foreach[A] {
  def head: A
  def tail: Linear[A]
  def isEmpty: Boolean
  @inline final def foreach(f: A => Unit): Unit = if (!isEmpty) { f(head) ; tail foreach f }
}

trait HasUnderlying[+Underlying] extends Any { def xs: Underlying }

object Foreach extends ForeachImplicits {
  object Empty extends PreciselySizedIndexed[Nothing](Zero) with HasStaticSize {
    def elemAt(index: Index): Nothing = failEmpty(ss"$this($index)")
    override def toString = "<empty>"
  }

  final class Constant[A](elem: A) extends Foreach[A] {
    def sizeInfo = Infinite
    @inline def foreach(f: A => Unit): Unit = while (true) f(elem)
    override def toString = ss"constant($elem)"
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
    override def toString = ss"unfold from $zero"
  }

  final case class Times[A](size: Size, elem: A) extends Foreach[A] with HasPreciseSize {
    @inline def foreach(f: A => Unit): Unit = 0 until size.value foreach (_ => f(elem))
    override def toString = ss"$elem x$size"
  }

  final class PureForeach[+A](mf: Suspended[A], val sizeInfo: SizeInfo) extends Foreach[A] {
    @inline def foreach(f: A => Unit): Unit = mf(f)
    override def toString = ss"$mf"
  }

  def from(n: Int): Foreach[Int]       = unfold(n)(_ + 1)
  def from(n: Long): Foreach[Long]     = unfold(n)(_ + 1)
  def from(n: BigInt): Foreach[BigInt] = unfold(n)(_ + 1)

  def to(start: Int, last: Int): Foreach[Int]   = Indexed.to(start, last).toSizedList// sized Size(last - start + 1)
  def const[A](elem: A): Constant[A]            = new Constant(elem)
  def times[A](times: Int, elem: A): Foreach[A] = Times(Size(times), elem)

  def unfold[A](start: A)(next: A => A): Unfold[A]   = Unfold[A](start)(next)
  def traversable[A](xs: Traversable[A]): Foreach[A] = TraversableAsForeach[A](xs)

  def empty[A] : Foreach[A] = Empty
  def apply[A](mf: Suspended[A]): Foreach[A] = new PureForeach[A](mf, SizeInfo.Unknown)
  def elems[A](xs: A*): Foreach[A] = Indexed.elems(xs: _*)
}

final case class TraversableAsForeach[+A](underlying: Traversable[A]) extends Foreach[A] {
  def sizeInfo = underlying match {
    case xs: sc.IndexedSeq[_] => Precise(Size(xs.size))
    case xs                   => if (xs.isEmpty) precise(0) else precise(1).atLeast
  }
  def foreach(f: A => Unit): Unit = underlying foreach f
}

final case class ForeachAsTraversable[+A](underlying: Foreach[A]) extends sc.immutable.Traversable[A] {
  def foreach[U](f: A => U): Unit = underlying foreach (x => f(x))
}

trait ForeachImplicits {
  implicit def tuple2ToForeach[A](p: Tuple2[A, A]): Foreach[A]          = Foreach.elems(p._1, p._2)
  implicit def tuple3ToForeach[A](p: Tuple3[A, A, A]): Foreach[A]       = Foreach.elems(p._1, p._2, p._3)
  implicit def tuple4ToForeach[A](p: Tuple4[A, A, A, A]): Foreach[A]    = Foreach.elems(p._1, p._2, p._3, p._4)
  implicit def tuple5ToForeach[A](p: Tuple5[A, A, A, A, A]): Foreach[A] = Foreach.elems(p._1, p._2, p._3, p._4, p._5)

  implicit def implicitIndexedOps[A](xs: Indexed[A]): IndexedOperations[A]          = new IndexedOperations(xs)
  implicit def implicitForeachOps[A](xs: Foreach[A]): ForeachOperations[A]          = new ForeachOperations(xs)
  implicit def implicitIndexedConversions[A](xs: Indexed[A]): IndexedConversions[A] = new IndexedConversions(xs)
  implicit def implicitForeachConversions[A](xs: Foreach[A]): ForeachConversions[A] = new ForeachConversions(xs)

  // Not recommended, but included for demo purposes.
  implicit def implicitlyForceReprView[Coll, A, That](xs: View[Coll, A])(implicit cb: CBF[Coll, A, That]): That = xs.force
}
