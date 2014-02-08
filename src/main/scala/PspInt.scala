package psp
package core

import java.{ lang => jl }

final class PspInt(val self: Int) extends AnyVal with Ordered[Int] {
  def compare(rhs: Int): Int = jl.Integer.compare(self, rhs)

  def abs: Int            = math.abs(self)
  def max(that: Int): Int = math.max(self, that)
  def min(that: Int): Int = math.min(self, that)
  def signum: Int         = math.signum(self)

  def binary: String = jl.Integer.toBinaryString(self)
  def hex: String    = jl.Integer.toHexString(self)
  def octal: String  = jl.Integer.toOctalString(self)

  def until(end: Int): IntRange        = IntRange.until(self, end)
  def to(end: Int): IntRange           = IntRange.to(self, end)
  def times[A](body: => A): Foreach[A] = Direct.fill(self)(body)
}
