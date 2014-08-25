package psp
package std

import java.{ lang => jl }

final class UInt private (val bits: Int) extends AnyVal with Ordered[UInt] {
  private type This = UInt

  def compare(that: UInt): Int = (value compare that.value).intValue
  def max(that: This): This = UInt(math.max(value, that.value))
  def min(that: This): This = UInt(math.min(value, that.value))

  def binary: String = value.binary
  def hex: String    = value.hex
  def octal: String  = value.octal

  def unary_~ : This   = UInt(~bits)
  def |(x: This): This = UInt(bits | x.bits)
  def &(x: This): This = UInt(bits & x.bits)
  def ^(x: This): This = UInt(bits ^ x.bits)
  def +(x: This): This = UInt(value + x.value)
  def *(x: This): This = UInt(value * x.value)

  def intValue: Int   = bits
  def longValue: Long = 0xFFFFFFFFL & bits
  def value: Long     = longValue
  override def toString = s"${value}u"
}
object UInt extends (Int => UInt) {
  final val Min = apply(0)
  final val Max = apply(0xFFFFFFFF)

  implicit val UIntEq   = Eq[UInt](_.value == _.value)
  implicit val UIntShow = Show[UInt](_.toString)

  def apply(x: Int): UInt  = new UInt(x)
  def apply(x: Long): UInt = new UInt(x.toInt)
}
