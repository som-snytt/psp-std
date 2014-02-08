package psp
package core

import impl._

trait Indexed[+A] extends Any with Foreach[A] {
  def isDefinedAt(index: Index): Boolean
  def elemAt(index: Index): A
  // TODO - move onto Ops
  def zip[B](that: Indexed[B]): Indexed[(A, B)]                                                       = zipWith(that)(_ -> _)
  def zipWith[A1, B](that: Indexed[A1])(f: (A, A1) => B): Indexed[B]                                  = new ZippedIndexed2(this, that, f)
  def zipWith[A1, A2, B](that1: Indexed[A1], that2: Indexed[A2])(f: (A, A1, A2) => B): Indexed[B] = new ZippedIndexed3(this, that1, that2, f)
}

trait Direct[+A] extends Any with Indexed[A] with HasPreciseSize
trait Invariant[A] extends Any
trait HasContains[A] extends Any with Invariant[A] { def contains(x: A): Boolean }
trait DirectLeaf[A] extends Any with Direct[A] with HasContains[A]

final class PureIndexed[+A](size: Size, indexFn: Int => A) extends IndexedImpl[A](size) {
  def elemAt(index: Index): A = indexFn(index)
}

object Indexed {
  implicit final class BippyBooOperations[A](val xs: Indexed[A]) extends AnyVal {
    def apply(index: Index): A = xs elemAt index
  }
}

object Direct {
  implicit def newBuilder[A] : Builds[A, Direct[A]] = Builds(_.toIndexed)

  implicit final class IndexedOperations[A](val xs: Direct[A]) extends AnyVal {
    def ++(ys: Direct[A]): Direct[A] = join(xs, ys)
  }

  object Empty extends IndexedImpl[Nothing](SizeInfo.Zero) with HasStaticSize[Nat._0] {
    def elemAt(index: Index): Nothing = failEmpty(pp"$this($index)")
    override def toString = "<empty>"
  }

  def join[A](xs: Direct[A], ys: Direct[A]): Direct[A] = pure(
    xs.size + ys.size,
    index => if (xs.size containsIndex index) xs elemAt index else ys elemAt index - xs.size.value
  )

  /** Immutability (particularly of Arrays) is on the honor system. */
  def pureArray[A](xs: Array[A]): Direct[A]                            = pure(Size(xs.length), xs apply _)
  def pure[Repr](xs: Repr)(implicit tc: DirectAccess[Repr]): Direct[tc.A] = pure(tc length xs, index => (tc elemAt xs)(index))
  def pure[A](size: Size, indexFn: Index => A): Direct[A]              = new PureIndexed(size, indexFn)

  def fill[A](times: Int)(body: => A): Direct[A] = {
    val buf = Vector.newBuilder[A]
    Interval(0, times) foreach (_ => buf += body)
    pure(buf.result)
  }
  def empty[A] : Direct[A] = Foreach.Empty
  def elems[A](xs: A*): Direct[A] = xs match {
    case xs: WrappedArray[A] => pureArray(xs.array)
    case _                   => pure(xs.toVector)
  }
}
object IntRange {
  def until(start: Int, end: Int): IntRange = if (end < start) until(start, start) else new IntRange(start, end - 1, isInclusive = false)
  def to(start: Int, last: Int): IntRange   = if (last < start) until(start, start) else new IntRange(start, last, isInclusive = true)
}

final class IntRange private (val start: Int, val last: Int, isInclusive: Boolean) extends IndexedImpl[Int](Size(last - start + 1)) with DirectLeaf[Int] {
  def contains(x: Int): Boolean = start <= x && x <= last
  def isEmpty               = last < start
  def end                   = last + 1
  def elemAt(i: Index): Int = start + i
  override def toString     = if (isInclusive) s"$start to $last" else s"$start until $end"
}
