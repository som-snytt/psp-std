package psp
package std

final class UInt private (val bits: Int) extends AnyVal {
  def max(that: UInt): UInt = UInt(longValue max that.longValue)
  def min(that: UInt): UInt = UInt(longValue min that.longValue)

  def binary: String = longValue.binary
  def hex: String    = longValue.hex
  def octal: String  = longValue.octal

  def unary_~ : UInt   = UInt(~bits)
  def |(x: UInt): UInt = UInt(bits | x.bits)
  def &(x: UInt): UInt = UInt(bits & x.bits)
  def ^(x: UInt): UInt = UInt(bits ^ x.bits)
  def +(x: UInt): UInt = UInt(longValue + x.longValue)
  def *(x: UInt): UInt = UInt(longValue * x.longValue)

  def intValue: Int   = bits
  def longValue: Long = UInt.mask32 & bits

  override def toString = s"${longValue}u"
}
object UInt extends (Int => UInt) {
  final val mask32 = 0xFFFFFFFFL
  final val Min: UInt = apply(0)
  final val Max: UInt = apply((1L << 32) - 1)

  implicit val UIntShow  = Show.natural[UInt]
  implicit val UIntOrder = orderBy[UInt](_.longValue)

  def apply(x: Int): UInt  = new UInt(x)
  def apply(x: Long): UInt = new UInt(x.toInt)
}
