package psp
package core

import java.{ lang => jl }

trait Indexed[+A] extends Any with Foreach[A] with HasPreciseSize {
  def elemAt(index: Index): A
  @inline final def foreach(f: A => Unit): Unit = {
    var i = 0
    while (i < size.value) { f(elemAt(i)) ; i += 1 }
  }
}
trait InvariantIndexed[A] extends Any with Indexed[A] {
  def contains(x: A): Boolean
}

final class PureIndexed[+A](val size: Size, val indexFn: Int => A) extends Indexed[A] {
  def elemAt(index: Index): A = indexFn(index)
  override def toString = pp"Indexed[$size]"
}

final class PspWrappedString(val wrapped: String) extends Traversable[Char] {
  def foreach[U](f: Char => U): Unit = wrapped.toCharArray foreach f
}

final class PspStringOps(val repr: String) extends AnyVal with Indexed[Char] {
  private def augment = Predef augmentString repr

  def stripMargin(marginChar: Char): String = augment stripMargin marginChar
  def stripMargin: String                   = stripMargin('|')

  def toInt: Int               = jl.Integer parseInt repr
  def toDouble: Double         = jl.Double parseDouble repr
  def toChars: Indexed[Char]   = new ImmutableArray(repr.toCharArray)
  def toBytes: Indexed[Byte]   = new ImmutableArray(repr.getBytes())
  def toLines: Indexed[String] = split(Regex(EOL))

  def split(re: Regex): Indexed[String] = re splits repr
  def contains(ch: Char): Boolean = (repr indexOf ch) >= 0
  def elemAt(index: Index): Char  = repr charAt index
  def size                        = Size(repr.length)
  def format(args : Any*): String = java.lang.String.format(toString, args map unwrapArg: _*)
  def * (n: Int): String          = join("")((repr nTimes n).toSeq: _*)

  def stripPrefix(pre: String): String = if (repr startsWith pre) repr.substring(pre.length) else repr

  private def unwrapArg(arg: Any): AnyRef = arg match {
    case x: ScalaNumber => x.underlying
    case x              => x.toRef
  }
  override def toString = repr
}

object Indexed {
  implicit def newBuilder[A] : PspCanBuild[A, Indexed[A]] = new PspCanBuildImpl(_.toIndexed)

  /** This is on the honor system. */
  def immutableArray[A](xs: Array[A]): Indexed[A] = new ImmutableArray(xs)

  def fill[A](times: Int)(body: => A): Indexed[A] = {
    val buf = Vector.newBuilder[A]
    Interval(0, times) foreach (_ => buf += body)
    new ImmutableVector(buf.result)
  }
  def empty[A] : Indexed[A] = Foreach.Empty
  def apply[A](size: Size, indexFn: Index => A): Indexed[A] = new PureIndexed(size, indexFn)
  def elems[A](xs: A*): Indexed[A] = xs match {
    case xs: WrappedArray[A] => new ImmutableArray(xs.array)
    case _                   => new ImmutableVector(xs.toVector)
  }
}

private[psp] abstract class PreciselySizedIndexed[+A](val size: Size) extends Indexed[A] with HasPreciseSize {
  override def toString = Foreach stringify this
}

object IntRange {
  def until(start: Int, end: Int): IntRange = if (end < start) until(start, start) else new IntRange(start, end - 1, isInclusive = false)
  def to(start: Int, last: Int): IntRange   = if (last < start) until(start, start) else new IntRange(start, last, isInclusive = true)
}

final class IntRange private (val start: Int, val last: Int, isInclusive: Boolean) extends InvariantIndexed[Int] with HasPreciseSize {
  def contains(x: Int): Boolean = start <= x && x <= last

  def isEmpty               = last < start
  def end                   = last + 1
  def size                  = Size(end - start)
  def elemAt(i: Index): Int = start + i
  override def toString     = if (isInclusive) s"$start to $last" else s"$start until $end"
}

final class ImmutableArray[+A](xs: Array[A]) extends PreciselySizedIndexed[A](Size(xs.length)) {
  def elemAt(index: Index): A = xs(index)
}
final class ImmutableVector[+A](xs: Vector[A]) extends PreciselySizedIndexed[A](Size(xs.length)) {
  def elemAt(index: Index): A = xs(index)
}
final class IndexedString(xs: String) extends PreciselySizedIndexed[Char](Size(xs.length)) {
  def elemAt(index: Index): Char = xs charAt index
}
final class ImmutableIndexedSeq[+A](xs: IndexedSeq[A]) extends PreciselySizedIndexed[A](Size(xs.length)) {
  def elemAt(index: Index): A = xs(index)
}

