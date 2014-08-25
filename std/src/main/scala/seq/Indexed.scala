package psp
package std

trait Invariant[A] extends Any
trait Indexed[+A] extends Any with Foreach[A] { def elemAt(index: Index): A }
trait Direct[+A] extends Any with Indexed[A] with HasPreciseSize
trait DirectLeaf[A] extends Any with Direct[A] with Invariant[A] { def contains(x: A): Boolean }

object Direct {
  abstract class IndexedImpl[+A](val size: Size) extends Direct[A] with HasPreciseSize {
    def sizeInfo = size
    @inline final def foreach(f: A => Unit): Unit = size foreachIndex (i => f(elemAt(i)))
    override def toString = Foreach.stringify(this)(Show.native[A])
  }
  final class Pure[+A](size: Size, indexFn: Index => A) extends IndexedImpl[A](size) {
    def elemAt(index: Index): A = indexFn(index)
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
  def pureArray[A](xs: Array[A]): Direct[A]                               = pure(Size(xs.length), xs apply _.value)
  def pure[Repr](xs: Repr)(implicit tc: DirectAccess[Repr]): Direct[tc.A] = pure(tc length xs, index => (tc elemAt xs)(index))
  def pure[A](size: Size, indexFn: Index => A): Direct[A]                 = new Pure(size, indexFn)

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

final class IntRange private (val start: Int, val last: Int, isInclusive: Boolean) extends Direct.IndexedImpl[Int](Size(last - start + 1)) with DirectLeaf[Int] {
  def contains(x: Int): Boolean = start <= x && x <= last
  def isEmpty               = last < start
  def end                   = last + 1
  def elemAt(i: Index): Int = start + i.value
  override def toString     = if (isInclusive) s"$start to $last" else s"$start until $end"
}
