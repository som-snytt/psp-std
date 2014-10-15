package psp
package std

final class Utf8(val bytes: Array[Byte]) extends AnyVal {
  def chars: Chars = scala.io.Codec fromUTF8 bytes
  def to_s: String = new String(chars)
  override def toString = to_s
}
