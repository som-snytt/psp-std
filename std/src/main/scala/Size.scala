package psp
package std

import Size._, api._

class OverflowException extends RuntimeException

final class SizeOps private (val value: Int) extends AnyVal with api.Size {
  def + (n: Size): Size     = if (isUndefined) undefined else (value + n.value) |> (sum => if (sum < value) undefined else Size(sum))
  def - (n: Size): Size     = if (isUndefined) undefined else Size(value - n.value)
  def * (n: Int): Size      = if (isUndefined) undefined else Size(value * n)
  def min(that: Size): Size = if (isUndefined || that.isUndefined) undefined else Size(value min that.value)
  def max(that: Size): Size = if (isUndefined || that.isUndefined) undefined else Size(value max that.value)

  def / (n: Int): Size          = if (isUndefined || n <= 0) undefined else Size(value / n)
  def % (n: Int): Size          = if (isUndefined || n <= 0) undefined else Size(value % n)
  def /% (n: Int): (Size, Size) = (this / n, this % n)

  def isZero                   = this == Zero
  def isUndefined              = value < 0
  def toIndexRange: IndexRange = if (isUndefined) IndexRange.undefined else IndexRange zeroTo lastIndex
  def toIndex: Index           = Index(value)
  def toScalaRange             = toIndexRange.toScalaRange
  def toInt: Int               = value
  def toLong: Long             = value
  def toOption: Option[Int]    = if (isUndefined) None else Some(toInt)
  def toInfo: SizeInfo         = if (isUndefined) SizeInfo.Unknown else Precise(this)

  @inline def foreachIndex(f: Index => Unit): Unit = toIndexRange foreach f
  def containsIndex(index: api.Index): Boolean     = !index.isUndefined && index <= lastIndex

  def lastIndex: Index = if (value <= 0) Index.undefined else Index(value - 1)

  override def toString = if (isUndefined) "undefined" else s"$value"
}
object SizeOps {
  def apply(n: Int): SizeOps = new SizeOps(n max 0)
}

// Size is^Wshould be its own unapply (value class bugs drove us out for now)
object Size {
  def undefined: Size = NoSize
  private case class Impl(value: Int) extends AnyVal with Size

  final val NoSize: Size = Impl(-1)
  final val Zero: Size   = Impl(0)
  final val One: Size    = Impl(1)
  final val Two: Size    = Impl(2)
  final val Three: Size  = Impl(3)
  final val Four: Size   = Impl(4)
  final val Five: Size   = Impl(5)

  def apply(n: Int): Size           = if (n <= 0) Zero else Impl(n)
  def unapply(s: Size): Option[Int] = s.toOption
}
