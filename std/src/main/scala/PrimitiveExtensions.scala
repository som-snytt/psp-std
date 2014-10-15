package psp
package std
package ops

import api._
import java.{ lang => jl }
import lowlevel._

final class AnyOps[A](val x: A) extends AnyVal {
  // Short decoded class name.
  def shortClass: String   = x.getClass.shortName
  def shortPackage: String = x.getClass.shortPackage

  // "Maybe we can enforce good programming practice with annoyingly long method names."
  def castTo[U] : U   = x.asInstanceOf[U]
  def toRef: AnyRef   = castTo[AnyRef]
  def isNull: Boolean = toRef eq null

  def reflect[B](m: jMethod)(args: Any*): B = m.invoke(x, args.map(_.castTo[AnyRef]): _*).castTo[B]

  // The famed forward pipe.
  @inline def |>[B](f: A => B): B       = f(x)
  @inline def doto(f: A => Unit): A     = sideEffect(f(x))
  @inline def sideEffect(body: Unit): A = x

  // Calling eq on Anys.
  def id_==(y: Any): Boolean = toRef eq y.toRef
  def id_## : Int            = identityHashCode(x)

  def requiring(p: Predicate[A]): Option[A] = if (p(x)) Some(x) else None
  def matchOr[B](alt: => B)(pf: A ?=> B): B = if (pf isDefinedAt x) pf(x) else alt
  def try_s(implicit z: TryShow[A]): String = z show x
  def any_s: String = x match {
    case null          => "null"
    case x: ShowDirect => x.to_s
    case _             => x.toString
  }
}

final class AnyRefOps[A <: AnyRef](val x: A) extends AnyVal {
  @inline def doto(f: A => Unit): x.type     = sideEffect(f(x))
  @inline def sideEffect(body: Unit): x.type = x
}

final class CharOps(val ch: Char) extends AnyVal {
  def prev: Char   = (ch - 1).toChar
  def next: Char   = (ch + 1).toChar
  def isAlphabetic = jl.Character isAlphabetic ch
  def isDigit      = jl.Character isDigit ch
  def toUpper      = jl.Character toUpperCase ch
  def toLower      = jl.Character toLowerCase ch
  def isUpper      = jl.Character isUpperCase ch
  def isLower      = jl.Character isLowerCase ch
  def isWhitespace = jl.Character isWhitespace ch
  def isControl    = jl.Character isISOControl ch
}

final class IntOps(val self: Int) extends AnyVal {
  private type This = Int

  def nth: Nth       = Nth(self)
  def index: Index   = Index(self)
  def offset: Offset = Offset(self)
  def size: Precise  = Precise(self)

  /** Make a 64-bit long by concatenating two 32-bit Ints.
   *  Retrieve the original Ints with lbits and rbits.
   */
  @inline def join(that: Int): Long = (self.toLong << 32) | that.toLong

  def abs: This             = scala.math.abs(self)
  def max(that: This): This = scala.math.max(self, that)
  def min(that: This): This = scala.math.min(self, that)
  def signum: This          = scala.math.signum(self)

  def until(end: Int): ExclusiveIntRange = intRange(self, end)
  def to(end: Int): ExclusiveIntRange    = nthRange(self, end)

  def times[A](expr: => A): Direct[A] = Direct.fill(self)(expr)
  def u: UInt                         = UInt(self)
  def binary: String                  = jl.Integer.toBinaryString(self)
  def hex: String                     = jl.Integer.toHexString(self)
  def octal: String                   = jl.Integer.toOctalString(self)
}

final class LongOps(val self: Long) extends AnyVal {
  private type This = Long

  def abs: This             = scala.math.abs(self)
  def max(that: This): This = scala.math.max(self, that)
  def min(that: This): This = scala.math.min(self, that)
  def signum: This          = scala.math.signum(self)

  def lbits: Int = (self >>> 32).toInt
  def rbits: Int = self.toInt

  def toUnsignedInt: UInt = UInt(self)
  def binary: String      = jl.Long.toBinaryString(self)
  def hex: String         = jl.Long.toHexString(self)
  def octal: String       = jl.Long.toOctalString(self)
}
