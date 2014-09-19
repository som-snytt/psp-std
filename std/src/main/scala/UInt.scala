package psp
package std

import java.{ lang => jl }

final class UInt private (val bits: Int) extends AnyVal {
  def compare(that: UInt): api.Cmp = Order difference longValue - that.longValue
  def max(that: UInt): UInt        = UInt(math.max(value, that.value))
  def min(that: UInt): UInt        = UInt(math.min(value, that.value))

  def binary: String = value.binary
  def hex: String    = value.hex
  def octal: String  = value.octal

  def unary_~ : UInt   = UInt(~bits)
  def |(x: UInt): UInt = UInt(bits | x.bits)
  def &(x: UInt): UInt = UInt(bits & x.bits)
  def ^(x: UInt): UInt = UInt(bits ^ x.bits)
  def +(x: UInt): UInt = UInt(value + x.value)
  def *(x: UInt): UInt = UInt(value * x.value)

  def intValue: Int   = bits
  def longValue: Long = 0xFFFFFFFFL & bits
  def value: Long     = longValue
  override def toString = s"${value}u"
}
object UInt extends (Int => UInt) {
  final val Min = apply(0)
  final val Max = apply(0xFFFFFFFF)

  implicit val UIntShow  = Show.native[UInt]
  implicit val UIntOrder = Order[UInt](_.longValue compared _.longValue)

  def apply(x: Int): UInt  = new UInt(x)
  def apply(x: Long): UInt = new UInt(x.toInt)
}
