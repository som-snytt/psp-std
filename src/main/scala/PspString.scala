package psp
package core

import java.{ lang => jl }

final class PspStringOps(val repr: String) extends AnyVal {
  private def augment = Predef augmentString repr

  def stripMargin(marginChar: Char): String = augment stripMargin marginChar
  def stripMargin: String                   = stripMargin('|')
  def stripPrefix(prefix: String): String   = if (repr startsWith prefix) repr substring prefix.length else repr
  def stripSuffix(suffix: String): String   = if (repr endsWith suffix) repr substring (0, repr.length - suffix.length) else repr

  def toInt: Int                  = jl.Integer parseInt repr
  def toDouble: Double            = jl.Double parseDouble repr
  def format(args : Any*): String = jl.String.format(toString, args map unwrapArg: _*)

  def toBytes: PspStringAsBytes = new PspStringAsBytes(repr)
  def toChars: PspStringAsChars = new PspStringAsChars(repr)
  def toLines: PspStringAsLines = new PspStringAsLines(repr)

  def regex: Regex                      = Regex(repr)
  def literal: Regex                    = regex.literal
  def split(re: Regex): DelimitedString = new DelimitedString(repr, re)

  def * (n: Int): Foreach[String] = repr nTimes n

  private def unwrapArg(arg: Any): AnyRef = arg match {
    case x: ScalaNumber => x.underlying
    case x              => x.toRef
  }
  override def toString = repr
}

final class PspStringAsBytes(val repr: String) extends AnyVal with InvariantIndexed[Byte] {
  private def bytes: Array[Byte] = repr.getBytes()

  def size: Size                             = Size(bytes.length)
  def elemAt(index: Index): Byte             = bytes(index)
  def isDefinedAt(index: Index): Boolean     = size containsIndex index
  @inline def foreach(f: Byte => Unit): Unit = Indexed pure bytes foreach f
  def contains(x: Byte): Boolean             = bytes contains x

  override def toString = pp"String(as $size bytes)"
}

final class PspStringAsChars(val repr: String) extends AnyVal with InvariantIndexed[Char] {
  private def chars: Array[Char] = repr.toCharArray

  def size                                   = Size(chars.length)
  def elemAt(index: Index): Char             = chars(index)
  def isDefinedAt(index: Index): Boolean     = size containsIndex index
  @inline def foreach(f: Char => Unit): Unit = Indexed pure chars foreach f
  def contains(x: Char): Boolean             = chars contains x

  override def toString = pp"String(as $size chars)"
}

final class PspStringAsLines(val repr: String) extends AnyVal with InvariantIndexed[String] {
  private def isEol(index: Int)   = index >= 0 && repr.startsWith(EOL, index)
  private def isStart(index: Int) = index == 0 || isEol(index - EOL.length)
  private def isEnd(index: Int)   = index == repr.length || isEol(index)

  // private
  def starts: Indexed[Int]      = 0 to repr.length filter isStart force
  def ends: Indexed[Int]        = 0 to repr.length filter isEnd force
  def ranges: Indexed[Interval] = starts.zipWith(ends)(Interval).toIndexed

  def indicesOfLine(lineno: Int): Interval = ranges elemAt lineno - 1
  def line(lineno: Int): String            = elemAt(lineno - 1)

  //   Indexed.elems(-1, 0) ++ (0 until repr.length collect { case i if repr.startsWith(EOL, i) => i + EOL.length } filterNot (_ >= repr.length))
  // }

  // Indexed.elems(-1) ++ strings.scanl(0)(_ + _.length + EOL.length)
  private def strings: Indexed[String]        = Regex(EOL) splits repr
  // private def indexOfLine(lineno: Int): Index = (
  //   if (starts containsIndex lineno - 1)

  //   if (lineno <= 1) return NoIndex

  //   var currentLine = 1
  //   var currentIndex = 0
  //   while (currentIndex < repr.length && currentLine < lineno) {
  //     if (repr.startsWith(EOL, currentIndex)) {
  //       currentLine += 1
  //       currentIndex += EOL.length
  //     }
  //     else currentIndex += 1
  //   }
  //   if (currentIndex >= repr.length) NoIndex else currentIndex
  // }

  // def slice(start: Int, end: Int): PspStringAsLines =
  def size                                          = strings.size
  def elemAt(index: Index): String                  = strings elemAt index
  def isDefinedAt(index: Index): Boolean            = size containsIndex index
  @inline def foreach(f: String => Unit): Unit      = strings foreach f
  def contains(x: String): Boolean                  = strings exists (_ == x)

  override def toString = pp"String(as $size lines)"
}

final class DelimitedString(repr: String, delimiter: Regex) extends InvariantIndexed[String] {
  private[this] val elements: Indexed[String] = delimiter splits repr

  def size                                     = elements.size
  def elemAt(index: Index): String             = elements elemAt index
  def isDefinedAt(index: Index): Boolean       = size containsIndex index
  @inline def foreach(f: String => Unit): Unit = elements foreach f
  def contains(x: String): Boolean             = elements exists (_ == x)

  override def toString = pp"String(delimited by $delimiter to size $size)"
}
