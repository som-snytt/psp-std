package psp
package core

import java.{ lang => jl }
import psp.std._

final class Line(val text: String) extends AnyVal {
  override def toString = text
}

final class PspStringOps(val repr: String) extends AnyVal {
  def toBytes: PspStringAsBytes = new PspStringAsBytes(repr)
  def toChars: PspStringAsChars = new PspStringAsChars(repr)
  def toLines: PspStringAsLines = new PspStringAsLines(repr)
  def toFile: jFile             = new jFile(repr)

  override def toString = repr
}

trait IndexedLeafImpl[A] extends Any with DirectLeaf[A] {
  def sizeInfo: Precise = Precise(size)
}

final class PspStringAsBytes(val repr: String) extends IndexedLeafImpl[Byte] {
  private[this] val bytes: Array[Byte] = repr.getBytes()

  def size: Size                             = Size(bytes.length)
  def elemAt(index: Index): Byte             = bytes(index.value)
  def isDefinedAt(index: Index): Boolean     = size containsIndex index
  @inline def foreach(f: Byte => Unit): Unit = Direct pure bytes foreach f
  def contains(x: Byte): Boolean             = bytes contains x

  override def toString = pp"String of $size bytes"
}

final class PspStringAsChars(val repr: String) extends AnyVal with IndexedLeafImpl[Char] {
  private def chars: Array[Char] = repr.toCharArray

  def size                                   = Size(chars.length)
  def elemAt(index: Index): Char             = repr charAt index.value
  def isDefinedAt(index: Index): Boolean     = size containsIndex index
  @inline def foreach(f: Char => Unit): Unit = Direct pure chars foreach f
  def contains(x: Char): Boolean             = chars contains x

  override def toString = pp"String of $size chars"
}

final class PspStringAsLines(val repr: String) extends AnyVal with IndexedLeafImpl[Line] {
  private def isEol(index: Int)   = index >= 0 && repr.startsWith(EOL, index)
  private def isStart(index: Int) = index == 0 || isEol(index - EOL.length)
  private def isEnd(index: Int)   = index == repr.length || isEol(index)
  private def starts: Direct[Int] = IntRange.to(0, repr.length) filter isStart force
  private def ends: Direct[Int]   = IntRange.to(0, repr.length) filter isEnd force
  private def lines: Direct[Line] = repr split EOL map (x => new Line(x)) force

  def ranges: Direct[IndexRange]             = starts.zipWith(ends)(indexRange).toIndexed
  def indicesOfLine(lineno: Int): IndexRange = ranges elemAt Nth(lineno)
  def line(lineno: Int): Line                = elemAt(Nth(lineno))
  def lineNumbers: IntRange                  = IntRange.to(1, size.value)

  def size                                   = lines.size
  def elemAt(index: Index): Line             = lines elemAt index
  def isDefinedAt(index: Index): Boolean     = size containsIndex index
  @inline def foreach(f: Line => Unit): Unit = lines foreach f
  def contains(x: Line): Boolean             = lines exists (_ == x)

  override def toString = lineNumbers map (i => "%4s  %s\n".format(i, line(i))) join ""
}
