package psp
package std

import api._
import java.{ lang => jl }
import StdEq.charEq
import Regex._
import java.util.regex.{ Pattern, Matcher }

/** Rather than struggle with ambiguities with Predef.augmentString, we'll
 *  bury it and reimplement what we want.
 */
final class PspStringOps(val self: String) extends AnyVal with ops.DocStringOps with ForceShowDirect {
  def r: Regex = Regex(self)
  def u: jUrl  = jUrl(self)

  private def unwrapArg(arg: Any): AnyRef = arg match {
    case x: ScalaNumber => x.underlying
    case x: AnyRef      => x
  }
  private def isEmpty = onull == ""
  private def onull   = if (self eq null) "" else self

  def size: IntSize = Precise(self.length)

  def retain(regex: Regex): String       = regex all self mkString ""
  def remove(regex: Regex): String       = regex matcher self replaceFirst ""
  def removeAll(regex: Regex): String    = regex matcher self replaceAll ""
  def remove(literal: String): String    = remove(Regex quote literal)
  def removeAll(literal: String): String = removeAll(Regex quote literal)

  def replaceChar(pair: (Char, Char)): String = self.replace(pair._1, pair._2)
  def replaceLiteral(sub: LiteralReplacement): String =
    (Regex quote sub.from) matcher self replaceAll (Matcher quoteReplacement sub.to)

  // def replaceLiteral(subs: LiteralReplacement*): String = subs.foldLeft(self) {
  //   case (res, LiteralReplacement(from, to)) =>
  //     (Regex quote from) matcher self replaceAll (Matcher quoteReplacement to)
  // }
  def replacePattern(subs: PatternReplacement*): String = subs.foldLeft(self) {
    case (res, PatternReplacement(from, to)) => from matcher self replaceAll to
  }

  def processEscapes: String = scala.StringContext processEscapes self
  def isNonEmptyDigits       = self matches """^[\d]+$"""
  def isAllWhitespace        = self matches """[\s]*"""
  def nonEmpty: Boolean      = onull.length > 0
  def capitalize: String     = mapNonEmpty(x => x splitAt 1.index match { case SplitView(l, r) => l.force.toUpperCase ~ r.force })

  def ~ (that: String): String    = self + that
  def * (n: Int): String          = this * n.size
  def * (n: Precise): String  = n timesConst self mkString ""
  def format(args : Any*): String = java.lang.String.format(self, args map unwrapArg: _*)
  def length                      = self.length

  def bytes: Array[Byte]             = self.getBytes
  def chars: Array[Char]             = self.toCharArray
  def wordSet: ExSet[String]         = words.naturalSet
  def lineVector: Direct[String]     = splitChar('\n')
  def dollarSegments: Direct[String] = splitChar('$')
  def dottedSegments: Direct[String] = splitChar('.')
  def slashSegments: Direct[String]  = splitChar('/')

  def containsChar(ch: Char): Boolean       = chars.m contains ch
  def splitChar(ch: Char): Direct[String]  = splitRegex(Regex quote ch.toString)
  def splitRegex(r: Regex): Direct[String] = r.pattern split self pvec
  def words: Direct[String]                = splitRegex(whitespace)

  def mapChars(pf: Char ?=> Char): String          = self map (c => if (pf isDefinedAt c) pf(c) else c) build
  def mapLines(f: Unary[String]): String           = mapSplit('\n')(f)
  def mapNonEmpty(f: Unary[String]): String        = if (isEmpty) "" else f(self)
  def mapSplit(ch: Char)(f: Unary[String]): String = splitChar(ch) map f mkString ch.toString
  def stripMargin(marginChar: Char): String        = mapLines(_ remove ("""^\s*[""" + marginChar + "]").r)

  private def dropSuffix(s: String, drop: String) = s remove drop.r.characterClass.ends

  def toInt: Int            = foldPrefix("0x")(jl.Integer parseInt self, s => jl.Integer.parseInt(s, 16))
  def toLong: Long          = foldPrefix("0x")(jl.Long parseLong dropSuffix(self, "lL"), s => jl.Long.parseLong(dropSuffix(s, "lL"), 16))
  def toDouble: Double      = jl.Double parseDouble dropSuffix(self, "dD")
  def toFloat: Float        = jl.Float parseFloat dropSuffix(self, "fF")
  def toBigInt: BigInt      = scala.math.BigInt(self)
  def toDecimal: BigDecimal = scala.math.BigDecimal(self)

  def readAs[A](implicit reads: Read[A]): A = reads read self

  def foldMatch[A](r: Regex)(none: => A, some: String => A): A        = (r first self).fold(none)(some)
  def foldPrefix[A](prefix: String)(none: => A, some: String => A): A = foldRemove(prefix.r.literal.starts)(none, some)
  def foldRemove[A](r: Regex)(none: => A, some: String => A): A       = remove(r) match { case `self` => none ; case s => some(s) }
  def foldSuffix[A](suffix: String)(none: => A, some: String => A): A = foldRemove(suffix.r.literal.ends)(none, some)
  def stripPrefix(prefix: String): String                             = foldPrefix(prefix)(self, s => s)
  def stripSuffix(suffix: String): String                             = foldSuffix(suffix)(self, s => s)

  def trimLines: String    = mapLines(_.trim)
  def trimTrailing: String = mapLines(_ remove whitespace.ends)
  def trimLeading: String  = mapLines(_ remove whitespace.starts)

  def stripAnsi: String   = removeAll(ansiCodes)
  def stripMargin: String = stripMargin('|')
  def sanitize: String    = mapChars { case x if x.isControl => '?' }

  def truncateAndLeftJustifyTo(max: Precise) = max leftFormat (normalizeSpace truncateTo max).asis
  def truncateTo(max: Precise)               = if ((size: Precise) <= max) self else (self take max - 3).force ~ "..."

  def normalizeSpace: String = self.trim.replacePattern(
    "\\n+"      -> "\n",
    "([{(])\\n" -> "$1 ",
    "\\n"       -> "; ",
    "\\s+"      -> " "
  )

  def to_s = self
}
