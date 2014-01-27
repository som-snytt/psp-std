package psp
package core

final class PspInt(val self: Int) extends AnyVal {
  def abs: Int            = math.abs(self)
  def max(that: Int): Int = math.max(self, that)
  def min(that: Int): Int = math.min(self, that)
  def signum: Int         = math.signum(self)

  def binary: String = java.lang.Integer.toBinaryString(self)
  def hex: String    = java.lang.Integer.toHexString(self)
  def octal: String  = java.lang.Integer.toOctalString(self)

  def until(end: Int): IntRange        = IntRange.until(self, end)
  def to(end: Int): IntRange           = IntRange.to(self, end)
  def times[A](body: => A): Foreach[A] = Indexed.fill(self)(body)
}
