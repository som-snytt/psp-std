package psp
package core

import psp.common.Labeled
import scala.{ collection => sc }
import SizeInfo._

trait Foreach[+A] extends Any with HasSizeInfo { def foreach(f: A => Unit): Unit }

class LabeledForeach[+A](xs: Foreach[A], val label: String) extends Foreach[A] with Labeled {
  def sizeInfo = xs.sizeInfo
  @inline final def foreach(f: A => Unit): Unit = xs foreach f
}

final case class SizedForeach[+A](xs: Foreach[A], sizeInfo: SizeInfo) extends Foreach[A] {
  def foreach(f: A => Unit): Unit = xs foreach f
  override def toString = ss"$xs"
}

sealed abstract case class Mapped[A, B](xs: Foreach[A], f: A => B) extends Foreach[B] {
  def sizeInfo = xs.sizeInfo
  @inline final def foreach(g: B => Unit): Unit = xs foreach (x => g(f(x)))
  override def toString = ss"$xs map $f"
}
final class LinearMapped[A, B](xs: Foreach[A], f: A => B) extends Mapped(xs, f)
final class IndexedMapped[A, B](xs: Indexed[A], f: A => B) extends Mapped(xs, f) with Indexed[B] {
  def apply(index: Index): B = f(xs(index))
}

object Foreach {
  object Empty extends Foreach[Nothing] {
    def sizeInfo = SizeInfo.Empty
    def foreach(f: Nothing => Unit): Unit = ()
    override def toString = "<empty>"
  }

  trait Infinite[A] extends Any with Foreach[A] { final def sizeInfo = Infinite }

  final class Unfold[A](zero: A, next: A => A) extends Infinite[A] {
    def foreach(f: A => Unit): Unit = {
      var current = zero
      while (true) {
        f(current)
        current = next(current)
      }
    }
    override def toString = ss"unfold from $zero"
  }

  final case class FlatMapped[A, B](xs: Foreach[A], f: A => Foreach[B]) extends Foreach[B] {
    def sizeInfo = if (SizeInfo(xs).isZero) SizeInfo.Empty else SizeInfo.Unknown
    def foreach(g: B => Unit): Unit = xs foreach (x => f(x) foreach g)
    override def toString = ss"$xs flatMap $f"
  }

  final case class Filtered[A](xs: Foreach[A], p: A => Boolean) extends Foreach[A] {
    def sizeInfo = SizeInfo(xs).atMost
    def foreach(f: A => Unit): Unit = xs foreach (x => if (p(x)) f(x))
    override def toString = ss"$xs filter $p"
  }

  final case class Collected[A, B](xs: Foreach[A], pf: PartialFunction[A, B]) extends Foreach[B] {
    def sizeInfo = SizeInfo(xs).atMost
    def foreach(f: B => Unit): Unit = xs foreach (x => if (pf isDefinedAt x) f(pf(x)))
    override def toString = ss"$xs collect $pf"
  }

  final class PureForeach[+A](mf: Suspended[A]) extends Foreach[A] {
    def sizeInfo = Unknown
    def foreach(f: A => Unit): Unit = mf(f)
    override def toString = ss"$mf"
  }

  implicit final class ForeachFoldOps[A](val xs: Foreach[A]) /*extends AnyVal*/ {
    def sum(implicit num: Numeric[A]): A     = foldl(num.zero)(num.plus)
    def product(implicit num: Numeric[A]): A = foldl(num.one)(num.times)
    def min(implicit ord: Ordering[A]): A    = reduce(ord.min)
    def max(implicit ord: Ordering[A]): A    = reduce(ord.max)

    def reduce(f: (A, A) => A): A = {
      var result: A = null.asInstanceOf[A]
      var first = true
      xs foreach (x => if (first) try result = x finally first = false else result = f(result, x))
      if (first) failEmpty("reduce") else result
    }
    def foldl[B](zero: B)(f: (B, A) => B): B          = {
      var result = zero
      xs.foreach(x => result = f(result, x))
      result
    }
    def foldr[B](zero: B)(f: (A, B) => B): B          = {
      var result = zero
      xs.foreach(x => result = f(x, result))
      result
    }
    def find(p: A => Boolean): Option[A] = { xs.foreach(x => if (p(x)) return Some(x)) ; None }
    def forall(p: A => Boolean): Boolean = { xs.foreach(x => if (!p(x)) return false) ; true }
    def exists(p: A => Boolean): Boolean = { xs.foreach(x => if (p(x)) return true) ; false }
  }

  implicit final class ForeachConversions[A](val xs: Foreach[A]) extends AnyVal {
    private def intSize: Int = SizeInfo(xs) match {
      case Precise(Size(n)) => n
      case _                => -1
    }
    def toVector: Vector[A]           = to[Vector]
    def toList: List[A]               = to[List]
    def toStream: Stream[A]           = toTraversable.toStream
    def toIterable: Iterable[A]       = toTraversable.toIterable
    def toTraversable: Traversable[A] = xs match {
      case xs: Indexed[_] if intSize >= 0 => IndexedAsTraversable(intSize, xs)
      case _                              => ForeachAsTraversable(xs)
    }
    def to[CC[X] <: Traversable[X]](implicit cbf: CanBuildFrom[Nothing, A, CC[A]]): CC[A]  = (cbf() ++= toTraversable).result
    def toRepr[Repr <: Traversable[A]](implicit cbf: CanBuildFrom[Nothing, A, Repr]): Repr = (cbf() ++= toTraversable).result

    def labeled(label: String): LabeledForeach[A] = new LabeledForeach(xs, label)

    // def toExtensionalSet(equiv: (A, A) => Boolean): ExtensionalSet[A] = new ExtensionalSet(xs, equiv)
  }

  def from(n: Int): Foreach[Int]       = unfold(n)(_ + 1)
  def from(n: Long): Foreach[Long]     = unfold(n)(_ + 1)
  def from(n: BigInt): Foreach[BigInt] = unfold(n)(_ + 1)

  def unfold[A](start: A)(next: A => A): Unfold[A]                           = new Unfold[A](start, next)
  def flatMapped[A, B](xs: Foreach[A])(f: A => Foreach[B]): FlatMapped[A, B] = new FlatMapped(xs, f)
  def collected[A, B](xs: Foreach[A])(pf: A =?> B): Collected[A, B]          = new Collected(xs, pf)
  def filtered[A](xs: Foreach[A])(f: A => Boolean): Filtered[A]              = new Filtered(xs, f)
  def traversable[A](xs: Traversable[A]): Foreach[A]                         = new TraversableAsForeach[A](xs)

  def empty[A] : Foreach[A] = Empty
  def apply[A](mf: Suspended[A]): Foreach[A] = new PureForeach[A](mf)
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

final case class IndexedAsTraversable[+A](length: Int, underlying: Indexed[A]) extends sc.immutable.IndexedSeq[A] {
  def apply(x: Int): A = underlying(x)
}
