package psp
package std

import java.{ lang => jl }

final class IntExtensionOps(val self: Int) extends AnyVal {
  private type This = Int

  def abs: This             = math.abs(self)
  def max(that: This): This = math.max(self, that)
  def min(that: This): This = math.min(self, that)
  def signum: This          = math.signum(self)

  def u: UnsignedInt = UnsignedInt(self)
  def binary: String = jl.Integer.toBinaryString(self)
  def hex: String    = jl.Integer.toHexString(self)
  def octal: String  = jl.Integer.toOctalString(self)
}

final class LongExtensionOps(val self: Long) extends AnyVal {
  private type This = Long

  def abs: This               = math.abs(self)
  def max(that: This): This   = math.max(self, that)
  def min(that: This): This   = math.min(self, that)
  def signum: This            = math.signum(self)

  def toUnsignedInt: UnsignedInt = UnsignedInt(self)
  def binary: String = jl.Long.toBinaryString(self)
  def hex: String    = jl.Long.toHexString(self)
  def octal: String  = jl.Long.toOctalString(self)
}

final class UnsignedInt private (val bits: Int) extends AnyVal {
  private type This = UnsignedInt

  def max(that: This): This = UnsignedInt(math.max(value, that.value))
  def min(that: This): This = UnsignedInt(math.min(value, that.value))
  def binary: String = value.binary
  def hex: String    = value.hex
  def octal: String  = value.octal

  def unary_~ : This   = UnsignedInt(~bits)
  def |(x: This): This = UnsignedInt(bits | x.bits)
  def &(x: This): This = UnsignedInt(bits & x.bits)
  def +(x: This): This = UnsignedInt(value + x.value)
  def *(x: This): This = UnsignedInt(value * x.value)

  def intValue: Int   = bits
  def longValue: Long = 0xFFFFFFFFL & bits
  def value: Long     = longValue
  override def toString = s"${value}u"
}
object UnsignedInt {
  final val Min = apply(0)
  final val Max = apply(0xFFFFFFFF)

  def apply(x: Int): UnsignedInt  = new UnsignedInt(x)
  def apply(x: Long): UnsignedInt = new UnsignedInt(x.toInt)
}
