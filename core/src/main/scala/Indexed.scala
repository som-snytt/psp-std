package psp
package core

trait Indexed[+A] extends Any with Foreach[A] { def apply(index: Index): A }
trait PreciselySizedIndexed[+A] extends Indexed[A] with HasPreciseSize

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
  final class Times[A](val size: Size, elem: A) extends PreciselySizedIndexed[A] {
    def apply(index: Index): A = elem
    @inline def foreach(f: A => Unit): Unit = 0 until size.value foreach (_ => f(elem))
    override def toString = ss"Times(size: $size)"
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
  def repeat[A](times: Int, elem: A): Times[A]  = new Times(Size(times), elem)
  def to(start: Int, last: Int): Indexed[Int]   = new IntRangeInclusive(start, Size(last - start + 1))
  // def until(start: Int, end: Int): Indexed[Int] = new IntRangeExclusive(start, end)
}
