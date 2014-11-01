package psp
package std

sealed trait IsDefault[+A] { def value: A }
final class DefaultFunction[+A](valueFn: => A) extends IsDefault[A] { def value = valueFn }
final class DefaultValue[+A](val value: A) extends IsDefault[A]

final case class Classpath(value: String) extends AnyVal { override def toString = value }
final case class Version(value: String) extends AnyVal   { override def toString = value }

final class Utf8(val bytes: Array[Byte]) extends AnyVal with ForceShowDirect {
  def chars: Chars = scala.io.Codec fromUTF8 bytes
  def to_s: String = new String(chars)
}
