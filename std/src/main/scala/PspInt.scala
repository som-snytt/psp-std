package psp
package std

import java.{ lang => jl }

final class IntExtensionOps(val self: Int) extends AnyVal with Ordered[Int] {
  private type This = Int
  def compare(rhs: This): This = Java6Compat.compare(self, rhs)
  def abs: This                = math.abs(self)
  def max(that: This): This    = math.max(self, that)
  def min(that: This): This    = math.min(self, that)
  def signum: This             = math.signum(self)

  def binary: String = jl.Integer.toBinaryString(self)
  def hex: String    = jl.Integer.toHexString(self)
  def octal: String  = jl.Integer.toOctalString(self)
}

final class LongExtensionOps(val self: Long) extends AnyVal with Ordered[Long] {
  private type This = Long
  def compare(rhs: This): Int = Java6Compat.compare(self, rhs)
  def abs: This               = math.abs(self)
  def max(that: This): This   = math.max(self, that)
  def min(that: This): This   = math.min(self, that)
  def signum: This            = math.signum(self)

  def binary: String = jl.Long.toBinaryString(self)
  def hex: String    = jl.Long.toHexString(self)
  def octal: String  = jl.Long.toOctalString(self)
}

/** These methods arrived in jl.{ Integer, Long } in java7. */
object Java6Compat {
  def compare(x: Int, y: Int): Int   = if (x < y) -1 else if (x == y) 0 else 1
  def compare(x: Long, y: Long): Int = if (x < y) -1 else if (x == y) 0 else 1
}
