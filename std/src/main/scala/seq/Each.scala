package psp
package std

import api._

sealed abstract class CollectionSizeException(msg: String) extends RuntimeException(msg)
final class InfiniteSizeException(msg: String) extends CollectionSizeException(msg)
final class LongSizeException(msg: String) extends CollectionSizeException(msg)

trait FromScala[Repr] extends Any {
  def scalaCollection: Repr
}
object FromScala {
  def apply[A, CC[X] <: sCollection[X]](xs: CC[A]): FromScala[CC[A]] = new FromScala[CC[A]] { def scalaCollection = xs }
  def unapply[Repr](wrapped: FromScala[Repr]): Some[Repr]            = Some(wrapped.scalaCollection)
}

object Each {
  def builder[A] : Builds[A, Each[A]] = Builds(identity)

  final class FromJava[A](xs: jIterable[A]) extends Each[A] {
    def size = Size(xs)
    @inline def foreach(f: A => Unit): Unit = BiIterable(xs) foreach f
  }
  final case class FromScala[A](scalaCollection: sCollection[A]) extends AnyVal with Each[A] with psp.std.FromScala[sCollection[A]] {
    def size = Size(scalaCollection)
    @inline def foreach(f: A => Unit): Unit = scalaCollection foreach f
  }
  /** We have to produce a scala Seq in order to return from an extractor.
   *  That requires us to produce made-up values for these methods thanks to
   *  scala's rampant overspecification.
   */
  final class ToScala[A](xs: Each[A]) extends sciSeq[A] {
    override def length: Int = xs.size match {
      case Infinite                 => throw new InfiniteSizeException(s"$xs")
      case Precise(n) if n > MaxInt => throw new LongSizeException(s"$xs")
      case Precise(n)               => n.toInt
    }
    def iterator: scIterator[A] = xs.memo.iterator
    def apply(index: Int): A = xs drop index.size head
    override def foreach[U](f: A => U): Unit = xs foreach (x => f(x))
  }
  final class Impl[A](val size: Size, mf: Suspended[A]) extends Each[A] {
    @inline def foreach(f: A => Unit): Unit = mf(f)
  }
  final case class Joined[A](xs: Each[A], ys: Each[A]) extends Each[A] {
    def size = xs.size + ys.size
    @inline def foreach(f: A => Unit): Unit = {
      xs foreach f
      ys foreach f
    }
  }
  trait AtomicSize[+A] extends Any with Each[A] with HasAtomicSize
  trait InfiniteSize[+A] extends Any with AtomicSize[A] {
    def isEmpty  = false
    def size = Infinite
  }
  object KnownSize {
    def unapply[A](xs: Each[A]) = xs.size optionally { case x: Atomic => x }
  }

  final case class Sized[A](underlying: Each[A], override val size: Precise) extends Each[A] with HasPreciseSize {
    def isEmpty = size.isZero
    @inline def foreach(f: A => Unit): Unit = {
      var count: Precise = 0.size
      underlying foreach { x =>
        if (count >= size) return
        f(x)
        count += 1
      }
    }
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

  def from(n: BigInt): Each[BigInt] = unfold(n)(_ + 1)
  def from(n: Int): Each[Int]       = unfold(n)(_ + 1)
  def from(n: Long): Each[Long]     = unfold(n)(_ + 1)
  def indices: Each[Index]          = unfold(Index(0))(_.next)

  def elems[A](xs: A*): Each[A]                                 = apply[A](xs foreach _)
  def const[A](elem: A): Constant[A]                            = Constant[A](elem)
  def continuallyWhile[A](p: Predicate[A])(expr: => A): Each[A] = continually(expr) takeWhile p
  def continually[A](elem: => A): Continually[A]                = Continually[A](() => elem)
  def empty[A] : Each[A]                                        = Direct.Empty
  def join[A](xs: Each[A], ys: Each[A]): Each[A]                = Joined[A](xs, ys)
  def unfold[A](start: A)(next: A => A): Unfold[A]              = Unfold[A](start)(next)

  def apply[A](mf: Suspended[A]): Each[A]        = new Impl[A](Size.unknown, mf)
  def unapplySeq[A](xs: Each[A]): Some[scSeq[A]] = Some(xs.seq)

  def show[A: Show](xs: Each[A], minElements: Precise, maxElements: Precise): String = xs splitAt maxElements.lastIndex match {
    case SplitView(xs, ys) if ys.isEmpty => xs mk_s ", "
    case SplitView(xs, _)                => (xs take minElements mk_s ", ") ~ ", ..."
  }
}
