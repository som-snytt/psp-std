package psp
package core

import Size._

class OverflowException extends RuntimeException

final class Size private (val value: Int) extends AnyVal with Ordered[Size] {
  private def checkSum(sum: Int): Size = try Size(sum) finally if (sum < value) fail(s"overflow: $value + ... == $sum")

  def compare(that: Size): Int = value compare that.value
  def + (n: Size): Size        = checkSum(value + n.value)
  def - (n: Size): Size        = Size(value - n.value)
  def * (n: Int): Size         = Size(value * n)
  def / (n: Int): Size         = if (n == 0) fail("division by zero") else Size(value / n)
  def min(that: Size): Size    = Size(value min that.value)
  def max(that: Size): Size    = Size(value max that.value)

  def isZero                        = this == Zero
  def isError                       = this == NoSize
  def toInterval: Interval          = Interval(0, value)
  def reverseInterval: Indexed[Int] = toInterval.reverse
  def toScalaRange                  = toInterval.toScalaRange
  def toIndexed: Indexed[Int]       = toInterval.toIndexed
  def toInt: Int                    = value
  def toLong: Long                  = value
  def toOption: Option[Int]         = if (isError) None else Some(toInt)
  def toInfo: Precise               = if (isError) fail(s"Cannot translate erroneous size") else Precise(this)

  @inline def foreachIndex(f: Index => Unit): Unit = toInterval foreach f
  def containsIndex(index: Index): Boolean = 0 <= index && index < value
  def lastIndex: Index = if (value <= 0) fail("empty.last") else value - 1

  override def toString = if (isError) "<no size>" else s"$value"
}

// Size is^Wshould be its own unapply (value class bugs drove us out for now)
object Size {
  implicit def sizeToSizeInfo(s: Size): SizeInfo = s.toInfo

  final val NoSize = new Size(-1)
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
