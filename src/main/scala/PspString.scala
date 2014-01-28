package psp
package core

import java.{ lang => jl }

final class PspStringOps(val repr: String) extends AnyVal with Indexed[Char] {
  private def augment = Predef augmentString repr

  def stripMargin(marginChar: Char): String = augment stripMargin marginChar
  def stripMargin: String                   = stripMargin('|')

  def toInt: Int               = jl.Integer parseInt repr
  def toDouble: Double         = jl.Double parseDouble repr
  def toChars: Indexed[Char]   = Indexed pure repr.toCharArray
  def toBytes: Indexed[Byte]   = Indexed pure repr.getBytes()
  def toLines: Indexed[String] = split(Regex(EOL))

  def isDefinedAt(index: Index): Boolean = 0 <= index && index < repr.length
  def foreach(f: Char => Unit): Unit = size.toInterval foreach (i => f(repr charAt i))
  def split(re: Regex): Indexed[String] = re splits repr
  def contains(ch: Char): Boolean = (repr indexOf ch) >= 0
  def size = Size(repr.length)
  def elemAt(index: Index): Char  = repr charAt index
  def format(args : Any*): String = java.lang.String.format(toString, args map unwrapArg: _*)
  def * (n: Int): String          = join("")((repr nTimes n).toSeq: _*)

  def stripPrefix(pre: String): String = if (repr startsWith pre) repr.substring(pre.length) else repr

  private def unwrapArg(arg: Any): AnyRef = arg match {
    case x: ScalaNumber => x.underlying
    case x              => x.toRef
  }
  override def toString = repr
}
