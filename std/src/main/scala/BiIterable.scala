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
  def empty[A] : BiIterator[A]                                       = Empty
  def single[A](x: A): BiIterator[A]                                 = new Single(x)
  def joined[A](xs: BiIterator[A], ys: BiIterator[A]): BiIterator[A] = new Joined(xs, ys)
  def mapped[A, B](it: BiIterator[A], f: A => B): BiIterator[B]      = new Mapped(it, f)
  def array[A](xs: Array[A]): BiIterator[A]                          = new ArrayIterator(xs)
  def linear[A](xs: Linear[A]): BiIterator[A]                        = new LinearIterator(xs)
  def vector[A](xs: Direct[A]): BiIterator[A]                       = new VectorIterator(xs)
  def enumeration[A](enum: jEnumeration[A]): BiIterator[A]           = new EnumerationIterator(enum)

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
  private class VectorIterator[A](xs: Direct[A]) extends BiIterator[A] {
    private[this] var index: Index = 0.index
    def hasNext   = xs.size containsIndex index
    def next(): A = try xs(index) finally index += 1
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

  private class VectorBased[A](xs: Direct[A])   extends BiIterableImpl(BiIterator vector xs)
  private class LinearBased[A](xs: Linear[A])    extends BiIterableImpl(BiIterator linear xs)
  private class ArrayBased[A](xs: Array[A])      extends BiIterableImpl(BiIterator array xs)
  private class JavaBased[A](xs: jIterable[A])   extends BiIterableImpl(xs.iterator |> (it => new Impl(it.hasNext, it.next)))
  private class ScalaBased[A](xs: scIterable[A]) extends BiIterableImpl(xs.iterator |> (it => new Impl(it.hasNext, it.next)))

  def apply[A](xs: Each[A]): BiIterable[A]       = new ScalaBased(xs.toScalaStream) // XXX
  def apply[A](xs: pList[A]): BiIterable[A]      = new LinearBased(xs)
  def apply[A](xs: Direct[A]): BiIterable[A]    = new VectorBased(xs)
  def apply[A](xs: Array[A]): BiIterable[A]      = new ArrayBased(xs)
  def apply[A](xs: jIterable[A]): BiIterable[A]  = new JavaBased(xs)
  def apply[A](xs: scIterable[A]): BiIterable[A] = new ScalaBased(xs)
  def elems[A](xs: A*): BiIterable[A]            = new VectorBased(xs.m.pvec)
}
