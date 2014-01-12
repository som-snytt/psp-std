package psp
package core

trait Indexed[+A] extends Any with Foreach[A] {
  def apply(index: Index): A
  def indexFn: Index => A = x => apply(x)
}
trait PreciselySizedIndexed[+A] extends Indexed[A] with HasPreciseSize

final case class WithIndex[A](xs: Foreach[A], fn: Index => A) extends Indexed[A] {
  def sizeInfo = xs.sizeInfo
  def apply(index: Index): A = fn(index)
  @inline def foreach(f: A => Unit): Unit = xs foreach f
  override def toString = ss"$xs/indexed"
}

final class ImmutableArray[+A](xs: Array[A]) extends PreciselySizedIndexed[A] {
  private[this] val len = xs.length
  def size = Size(len)
  @inline final def foreach(f: A => Unit): Unit = {
    var i = 0
    while (i < len) { f(xs(i)) ; i += 1 }
  }
  def apply(index: Index): A = xs(index)
  override def toString = ss"ImmutableArray($size)"
}

final class ImmutableVector[+A](xs: Vector[A]) extends PreciselySizedIndexed[A] {
  def size = Size(xs.length)
  def apply(index: Index): A = xs(index)
  @inline final def foreach(f: A => Unit): Unit = xs foreach f
  override def toString = ss"Vector($size)"
}

object Indexed {
  trait IndexedCommon[+A] extends PreciselySizedIndexed[A] {
    def size: Size
    private def intSize = size.value
    @inline final def foreach(f: A => Unit): Unit = {
      @tailrec def loop(i: Index): Unit = if (i < intSize) { f(apply(i)) ; loop(i + 1) }
      loop(0)
    }
    def apply(index: Index): A = {
      if (0 <= index && index < intSize)
        applyDirect(index)
      else
        throw new IndexOutOfBoundsException("" + index)
    }
    protected[this] def applyDirect(index: Index): A
  }

  object Empty extends PreciselySizedIndexed[Nothing] {
    def size = Zero
    def apply(index: Index): Nothing = failEmpty("Empty.apply")
    def foreach(f: Nothing => Unit): Unit = ()
    override def toString = "<empty>"
  }

  final class PureIndexed[+A](val size: Size, f: Index => A) extends PreciselySizedIndexed[A] {
    def apply(index: Index): A = f(index)
    @inline def foreach(f: A => Unit): Unit = 0 until size.value foreach (i => f(apply(i)))
    override def toString = ss"PureIndexed(size: $size)"
  }
  final case class Sliced[A](xs: Indexed[A], interval: Interval) extends IndexedCommon[A] {
    def size = interval.size
    protected[this] def applyDirect(index: Index): A = xs(interval.start + index)
    override def toString = ss"$xs.slice(${interval.start}, ${interval.end})"
  }
  final class IntRangeInclusive(start: Int, val size: Size) extends IndexedCommon[Int] {
    private def last: Int = start + size.value - 1
    protected[this] def applyDirect(index: Index): Int = start + index
    override def toString = ss"$start to $last"
  }
  final class ImmutableArray[+A](xs: Array[A]) extends IndexedCommon[A] {
    def size: Size = Size(xs.length)
    protected[this] def applyDirect(index: Int): A = xs(index)
    override def toString = ss"ImmutableArray($size)"
  }

  def apply[A](size: Size, f: Index => A): Indexed[A] = new PureIndexed(size, f)
  def elems[A](xs: A*): Indexed[A] = xs match {
    case xs: WrappedArray[A] => new ImmutableArray(xs.array)
    case _                   => apply(Size(xs.size), xs.toVector)
  }
  def to(start: Int, last: Int): Indexed[Int]   = new IntRangeInclusive(start, Size(last - start + 1))
  // def until(start: Int, end: Int): Indexed[Int] = new IntRangeExclusive(start, end)
}
