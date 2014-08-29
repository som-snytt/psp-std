package psp
package std

import api._

abstract class DirectLeaf[+A](val size: api.Size) extends Direct[A] with api.HasPreciseSize {
  def sizeInfo = Precise(Size(size.value))
  @inline final def foreach(f: A => Unit): Unit = size foreachIndex (i => f(elemAt(i)))
  override def toString = Foreach.stringify(this)(Show.native[A])
}

object Direct {
  final class Pure[+A](size: api.Size, indexFn: api.Index => A) extends DirectLeaf[A](size) {
    def elemAt(index: api.Index): A = indexFn(index)
  }
  object Empty extends DirectLeaf[Nothing](SizeInfo.Zero) with HasStaticSize[Nat._0] {
    def elemAt(index: api.Index): Nothing = failEmpty(pp"$this($index)")
    override def toString = "<empty>"
  }

  def join[A](xs: Direct[A], ys: Direct[A]): Direct[A] = pure(
    (xs.size: Size) + ys.size,
    index => if (xs.size containsIndex index) xs elemAt index else ys elemAt index - xs.size.value
  )

  /** Immutability (particularly of Arrays) is on the honor system. */
  def pureArray[A](xs: Array[A]): Direct[A]                               = pure(Size(xs.length), xs apply _.value)
  def pure[Repr](xs: Repr)(implicit tc: DirectAccess[Repr]): Direct[tc.A] = pure(tc length xs, index => (tc elemAt xs)(index))
  def pure[A](size: Size, indexFn: api.Index => A): Direct[A]             = new Pure(size, indexFn)

  def fill[A](times: Int)(body: => A): Direct[A] = {
    val buf = Vector.newBuilder[A]
    indexRange(0, times) foreach (_ => buf += body)
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

final class IntRange private (val start: Int, val last: Int, isInclusive: Boolean) extends DirectLeaf[Int](Size(last - start + 1)) {
  def contains(x: Int): Boolean = start <= x && x <= last
  def isEmpty                   = last < start
  def end                       = last + 1
  def elemAt(i: api.Index): Int = start + i.value
  override def toString         = if (isInclusive) s"$start to $last" else s"$start until $end"
}
