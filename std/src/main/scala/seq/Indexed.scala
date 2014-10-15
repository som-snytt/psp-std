package psp
package std

import api._

abstract class DirectLeaf[+A](val size: Size) extends Direct[A] with HasPreciseSize {
  def sizeInfo                                  = Precise(Size(size.value))
  @inline final def foreach(f: A => Unit): Unit = size foreachIndex (i => f(elemAt(i)))
  override def toString                         = Foreach.stringify(this)(Show.native[A])
}

object Direct {
  def builder[A] : Builds[A, Direct[A]] = arrayBuilder[Any] map (res => new WrapArray[A](res))
  def stringBuilder(): Builds[Char, String] = arrayBuilder[Char] map (cs => new String(cs))
  def arrayBuilder[A: CTag]: Builds[A, Array[A]] = Builds[A, Array[A]](xs =>
    xs.sizeInfo match {
      case Precise(Size(size)) => new Array[A](size) doto (arr => xs foreachCounted ((i, x) => arr(i.value) = x))
      case _                   => scala.Array.newBuilder[A] doto (buf => xs foreach (x => buf += x)) result
    }
  )
  private class WrapString(xs: String) extends DirectLeaf[Char](xs.length.size) {
    def elemAt(i: Index): Char = xs charAt i.indexValue
  }
  private class WrapArray[A](xs: Array[_]) extends DirectLeaf[A](xs.length.size) {
    def elemAt(i: Index): A = xs(i.indexValue).castTo[A]
  }
  private class WrapScalaSeq[A](xs: sciIndexedSeq[A]) extends DirectLeaf[A](xs.length.size) {
    def elemAt(i: Index): A = xs(i.indexValue)
  }
  final class Pure[+A](size: Size, indexFn: Index => A) extends DirectLeaf[A](size) {
    def elemAt(index: api.Index): A = indexFn(index)
  }
  object Empty extends DirectLeaf[Nothing](SizeInfo.Zero) with HasStaticSize[Nat._0] {
    def elemAt(index: api.Index): Nothing = failEmpty(pp"empty($index)")
    override def toString = "<empty>"
  }

  def join[A](xs: Direct[A], ys: Direct[A]): Direct[A] = pure(
    (xs.size: Size) + ys.size,
    index => if (xs.size containsIndex index) xs elemAt index else ys elemAt index - xs.size.value
  )

  /** Immutability (particularly of Arrays) is on the honor system. */
  def pureArray[A](xs: Array[A]): Direct[A]                               = pure(Size(xs.length), xs apply _.value)
  def pure[Repr](xs: Repr)(implicit tc: DirectAccess[Repr]): Direct[tc.A] = pure(tc length xs, index => (tc elemAt xs)(index))
  def pure[A](size: Size, indexFn: Index => A): Direct[A]                 = new Pure(size, indexFn)

  def fill[A](times: Int)(body: => A): Direct[A] = elems((0 until times).toSeq map (_ => body): _*)
  def empty[A] : Direct[A] = Foreach.Empty
  def elems[A](xs: A*): Direct[A] = xs match {
    case xs: scmWrappedArray[A] => pureArray(xs.array)
    case _                      => pure(xs.toVector)
  }
}
object IntRange {
  val empty = new IntRange(0, -1, isInclusive = false)

  def until(start: Int, end: Int): IntRange = if (end <= start) empty else new IntRange(start, end - 1, isInclusive = false)
  def to(start: Int, last: Int): IntRange   = if (last < start) empty else new IntRange(start, last, isInclusive = true)
}

final class IntRange private (val start: Int, val last: Int, isInclusive: Boolean) extends DirectLeaf[Int](Size(last - start + 1)) {
  def contains(x: Int): Boolean = start <= x && x <= last
  def isEmpty                   = last < start
  def end                       = last + 1
  def elemAt(i: api.Index): Int = start + i.value
  override def toString         = if (isInclusive) s"$start to $last" else s"$start until $end"
}
