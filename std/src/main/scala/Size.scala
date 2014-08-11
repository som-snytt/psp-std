package psp
package std

import Size._

class OverflowException extends RuntimeException

final class Size private (val value: Int) extends AnyVal with Ordered[Size] {
  def compare(that: Size): Int = value compare that.value

  def + (n: Size): Size     = if (isUndefined) undefined else (value + n.value) |> (sum => if (sum < value) undefined else Size(sum))
  def - (n: Size): Size     = if (isUndefined) undefined else Size(value - n.value)
  def * (n: Int): Size      = if (isUndefined) undefined else Size(value * n)
  def min(that: Size): Size = if (isUndefined || that.isUndefined) undefined else Size(value min that.value)
  def max(that: Size): Size = if (isUndefined || that.isUndefined) undefined else Size(value max that.value)

  def / (n: Int): Size          = if (isUndefined || n <= 0) undefined else Size(value / n)
  def % (n: Int): Size          = if (isUndefined || n <= 0) undefined else Size(value % n)
  def /% (n: Int): (Size, Size) = (this / n, this % n)

  def isZero                   = this == Zero
  def isUndefined              = this == undefined
  def toIndexRange: IndexRange = if (isUndefined) IndexRange.undefined else IndexRange zeroTo lastIndex
  def toScalaRange             = toIndexRange.toScalaRange
  def toInt: Int               = value
  def toLong: Long             = value
  def toOption: Option[Int]    = if (isUndefined) None else Some(toInt)
  def toInfo: Precise          = if (isUndefined) fail(s"Cannot translate erroneous size") else Precise(this)

  @inline def foreachIndex(f: Index => Unit): Unit = toIndexRange foreach f
  def containsIndex(index: Index): Boolean         = !index.isUndefined && index <= lastIndex

  def lastIndex: Index = if (value <= 0) Index.undefined else Index(value - 1)

  override def toString = if (isUndefined) "undefined" else s"$value"
}

// Size is^Wshould be its own unapply (value class bugs drove us out for now)
object Size {
  implicit def sizeToSizeInfo(s: Size): SizeInfo = s.toInfo

  def undefined = new Size(-1)

  final val NoSize = undefined
  final val Zero   = new Size(0)
  final val One    = new Size(1)
  final val Two    = new Size(2)
  final val Three  = new Size(3)
  final val Four   = new Size(4)
  final val Five   = new Size(5)

  def apply(n: Int): Size           = if (n <= 0) Zero else new Size(n)
  def unapply(s: Size): Option[Int] = s.toOption

  private def fail(msg: String): Nothing = throw new ArithmeticException(msg)
}
