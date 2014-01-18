package psp
package core

trait Indexed[+A] extends Any with Foreach[A] with HasPreciseSize {
  def elemAt(index: Index): A
  def indices = 0 until size.value
  @inline final def foreach(f: A => Unit): Unit = indices foreach (i => f(elemAt(i)))
}

object Indexed {
  def empty[A] : Indexed[A] = Foreach.Empty
  def until(start: Int, end: Int): Indexed[Int] = if (end <= start) empty[Int] else new IntRange(start, end, end - 1)
  def to(start: Int, last: Int): Indexed[Int]   = if (last < start) empty[Int] else new IntRange(start, last + 1, last)
  def elems[A](xs: A*): Indexed[A] = xs match {
    case xs: WrappedArray[A] => new ImmutableArray(xs.array)
    case _                   => new ImmutableVector(xs.toVector)
  }
}

private[psp] abstract class PreciselySizedIndexed[+A](val size: Size) extends Indexed[A] with HasPreciseSize

final class IntRange(start: Int, end: Int, last: Int) extends PreciselySizedIndexed[Int](Size(end - start)) {
  require(last + 1 == end, this)
  def elemAt(i: Index): Int = start + i
  override def toString = ss"$start until $end"
}
final class ImmutableArray[+A](xs: Array[A]) extends PreciselySizedIndexed[A](Size(xs.length)) {
  def elemAt(index: Index): A = xs(index)
  override def toString = ss"ImmutableArray($size)"
}
final class ImmutableVector[+A](xs: Vector[A]) extends PreciselySizedIndexed[A](Size(xs.length)) {
  def elemAt(index: Index): A = xs(index)
  override def toString = ss"Vector($size)"
}
