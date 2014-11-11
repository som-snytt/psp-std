package psp
package std

import api._

/** Classes which implement both the java and scala interfaces.
 */
trait BiIterable[+A] extends jIterable[A @uV] with scIterable[A] {
  def iterator(): BiIterator[A]
}
trait BiIterator[+A] extends jIterator[A @uV] with scIterator[A] {
  def ++[A1 >: A](that: BiIterator[A1]): BiIterator[A1] = BiIterator.joined(this, that)
  override def map[B](f: A => B): BiIterator[B]         = BiIterator.mapped(this, f)

  def remove(): Unit = unsupportedOperationException("not supported")
  def hasNext: Boolean
  def next(): A
}

object BiIterator {
  def empty[A] : BiIterator[A] = Empty
  def apply[A](xs: Each[A]): BiIterator[A] = xs match {
    case xs: Direct[A]  => direct(xs)
    case xs: Indexed[A] => indexed(xs)
    case xs: Linear[A]  => linear(xs)
    case _              => direct(xs.toDirect) // XXX
  }

  def array[A](xs: Array[A]): BiIterator[A]                          = new ArrayIterator(xs)
  def direct[A](xs: Direct[A]): BiIterator[A]                        = new DirectIterator(xs)
  def enumeration[A](enum: jEnumeration[A]): BiIterator[A]           = new EnumerationIterator(enum)
  def indexed[A](xs: Indexed[A]): BiIterator[A]                      = new IndexedIterator(xs)
  def joined[A](xs: BiIterator[A], ys: BiIterator[A]): BiIterator[A] = new Joined(xs, ys)
  def linear[A](xs: Linear[A]): BiIterator[A]                        = new LinearIterator(xs)
  def mapped[A, B](it: BiIterator[A], f: A => B): BiIterator[B]      = new Mapped(it, f)
  def single[A](x: A): BiIterator[A]                                 = new Single(x)

  final class EnumerationIterator[A](enum: jEnumeration[A]) extends BiIterator[A] {
    def hasNext = enum.hasMoreElements
    def next()  = enum.nextElement()
  }

  private object Empty extends BiIterator[Nothing] {
    def hasNext         = false
    def next(): Nothing = illegalArgumentException("next on empty iterator")
  }
  private class LinearIterator[A](xs: Linear[A]) extends BiIterator[A] {
    private[this] var current: LazyCell = new LazyCell(xs)
    private class LazyCell(body: => Linear[A]) { lazy val v = body }
    def hasNext   = !current.v.isEmpty
    def next(): A = current.v |> (cur => try cur.head finally current = new LazyCell(cur.tail))
  }
  private class Mapped[A, B](it: BiIterator[A], f: A => B) extends BiIterator[B] {
    def hasNext   = it.hasNext
    def next(): B = f(it.next)
  }
  private class Joined[A](xs: BiIterator[A], ys: BiIterator[A]) extends BiIterator[A] {
    def hasNext   = xs.hasNext || ys.hasNext
    def next(): A = if (xs.hasNext) xs.next else ys.next
  }
  private class ArrayIterator[A](xs: Array[A]) extends BiIterator[A] {
    private[this] var index = 0
    def hasNext   = index < xs.length
    def next(): A = try xs(index) finally index += 1
  }
  private class DirectIterator[A](xs: Direct[A]) extends BiIterator[A] {
    private[this] var index: Index = 0.index
    def hasNext   = xs.size containsIndex index
    def next(): A = try xs(index) finally index += 1
  }
  private class IndexedIterator[A](xs: Indexed[A]) extends BiIterator[A] {
    private[this] var index: Index = 0.index
    def hasNext   = true // XXX - put containsIndex on Indexed/Direct
    def next(): A = try xs elemAt index finally index += 1
  }
  private class Single[A](x: A) extends BiIterator[A] {
    private[this] var done = false
    def hasNext   = !done
    def next(): A = if (done) Empty.next else try x finally done = true
  }
}

object BiIterable {
  private class BiIterableImpl[A](f: => BiIterator[A]) extends BiIterable[A] { def iterator() = f }
  private class Impl[A](hasNextFn: => Boolean, nextFn: => A) extends BiIterator[A] {
    def hasNext   = hasNextFn
    def next(): A = nextFn
  }

  private class DirectBased[A](xs: Direct[A])    extends BiIterableImpl(BiIterator direct xs)
  private class IndexedBased[A](xs: Indexed[A])  extends BiIterableImpl(BiIterator indexed xs)
  private class LinearBased[A](xs: Linear[A])    extends BiIterableImpl(BiIterator linear xs)
  private class ArrayBased[A](xs: Array[A])      extends BiIterableImpl(BiIterator array xs)
  private class JavaBased[A](xs: jIterable[A])   extends BiIterableImpl(xs.iterator |> (it => new Impl(it.hasNext, it.next)))
  private class ScalaBased[A](xs: scIterable[A]) extends BiIterableImpl(xs.iterator |> (it => new Impl(it.hasNext, it.next)))

  def apply[A](xs: Each[A]): BiIterable[A] = xs match {
    case xs: Direct[A]  => apply[A](xs)
    case xs: Indexed[A] => apply[A](xs)
    case xs: Linear[A]  => apply[A](xs)
    case _              => apply[A](xs.toScalaStream)
  }
  def apply[A](xs: Linear[A]): BiIterable[A]     = new LinearBased(xs)
  def apply[A](xs: Direct[A]): BiIterable[A]     = new DirectBased(xs)
  def apply[A](xs: Indexed[A]): BiIterable[A]    = new IndexedBased(xs)
  def apply[A](xs: Array[A]): BiIterable[A]      = new ArrayBased(xs)
  def apply[A](xs: jIterable[A]): BiIterable[A]  = new JavaBased(xs)
  def apply[A](xs: scIterable[A]): BiIterable[A] = new ScalaBased(xs)
  def elems[A](xs: A*): BiIterable[A]            = new DirectBased(xs.m.toDirect)
}
