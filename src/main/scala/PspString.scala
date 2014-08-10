package psp
package core

import java.{ lang => jl }

final class Line(val text: String) extends AnyVal {
  override def toString = text
}

final class PspStringOps(val repr: String) extends AnyVal {
  private def augment = Predef augmentString repr

  def toBytes: PspStringAsBytes = new PspStringAsBytes(repr)
  def toChars: PspStringAsChars = new PspStringAsChars(repr)
  def toLines: PspStringAsLines = new PspStringAsLines(repr)
  // def toPath: jPath             = path(repr)
  def toFile: jFile             = file(repr)

  def regex: Regex                      = Regex(repr)
  def literal: Regex                    = regex.literal
  def split(re: Regex): DelimitedString = new DelimitedString(repr, re)

  override def toString = repr
}

trait IndexedLeafImpl[A] extends Any with DirectLeaf[A] {
  def sizeInfo: Precise = Precise(size)
}

final class PspStringAsBytes(val repr: String) extends IndexedLeafImpl[Byte] {
  private[this] val bytes: Array[Byte] = repr.getBytes()

  def size: Size                             = Size(bytes.length)
  def elemAt(index: Index): Byte             = bytes(index)
  def isDefinedAt(index: Index): Boolean     = size containsIndex index
  @inline def foreach(f: Byte => Unit): Unit = Direct pure bytes foreach f
  def contains(x: Byte): Boolean             = bytes contains x

  override def toString = pp"String of $size bytes"
}

final class PspStringAsChars(val repr: String) extends AnyVal with IndexedLeafImpl[Char] {
  private def chars: Array[Char] = repr.toCharArray

  def size                                   = Size(chars.length)
  def elemAt(index: Index): Char             = repr charAt index
  def isDefinedAt(index: Index): Boolean     = size containsIndex index
  @inline def foreach(f: Char => Unit): Unit = Direct pure chars foreach f
  def contains(x: Char): Boolean             = chars contains x

  override def toString = pp"String of $size chars"
}

final class PspStringAsLines(val repr: String) extends AnyVal with IndexedLeafImpl[Line] {
  private def isEol(index: Int)       = index >= 0 && repr.startsWith(EOL, index)
  private def isStart(index: Int)     = index == 0 || isEol(index - EOL.length)
  private def isEnd(index: Int)       = index == repr.length || isEol(index)
  private def starts: Direct[Int]     = IntRange.to(0, repr.length) filter isStart force
  private def ends: Direct[Int]       = IntRange.to(0, repr.length) filter isEnd force
  private def strings: Direct[String] = Regex(EOL) splits repr
  private def lines: Direct[Line]     = strings map (x => new Line(x)) force

  def ranges: Direct[Interval]             = starts.zipWith(ends)((x, y) => Interval(x, y)).toIndexed
  def indicesOfLine(lineno: Int): Interval = ranges elemAt lineno - 1
  def line(lineno: Int): Line              = elemAt(lineno - 1)
  def lineNumbers: IntRange                = IntRange.to(1, size.value)

  def size                                   = strings.size
  def elemAt(index: Index): Line             = lines elemAt index
  def isDefinedAt(index: Index): Boolean     = size containsIndex index
  @inline def foreach(f: Line => Unit): Unit = lines foreach f
  def contains(x: Line): Boolean             = lines exists (_ == x)

  override def toString = lineNumbers map (i => "%4s  %s\n".format(i, line(i))) join ""
}

final class DelimitedString(repr: String, delimiter: Regex) extends IndexedLeafImpl[String] {
  private[this] val elements: Direct[String] = delimiter splits repr

  def size                                     = elements.size
  def elemAt(index: Index): String             = elements elemAt index
  def isDefinedAt(index: Index): Boolean       = size containsIndex index
  @inline def foreach(f: String => Unit): Unit = elements foreach f
  def contains(x: String): Boolean             = elements exists (_ == x)

  override def toString = pp"String(delimited by $delimiter to size $size)"
}
