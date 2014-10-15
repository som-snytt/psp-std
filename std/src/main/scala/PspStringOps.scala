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
final class PspStringOps(val xs: String) extends AnyVal {
  def r: Regex = Regex(xs)
  def u: jUrl  = jUrl(xs)

  private def unwrapArg(arg: Any): AnyRef = arg match {
    case x: ScalaNumber => x.underlying
    case x: AnyRef      => x
  }
  private def isEmpty = onull == ""
  private def onull   = if (xs eq null) "" else xs

  def remove(regex: Regex): String       = regex matcher xs replaceFirst ""
  def removeAll(regex: Regex): String    = regex matcher xs replaceAll ""
  def remove(literal: String): String    = remove(Regex quote literal)
  def removeAll(literal: String): String = removeAll(Regex quote literal)

  def replaceChar(pair: (Char, Char)): String = xs.replace(pair._1, pair._2)
  def replaceLiteral(sub: LiteralReplacement): String =
    (Regex quote sub.from) matcher xs replaceAll (Matcher quoteReplacement sub.to)

  // def replaceLiteral(subs: LiteralReplacement*): String = subs.foldLeft(xs) {
  //   case (res, LiteralReplacement(from, to)) =>
  //     (Regex quote from) matcher xs replaceAll (Matcher quoteReplacement to)
  // }
  def replacePattern(subs: PatternReplacement*): String = subs.foldLeft(xs) {
    case (res, PatternReplacement(from, to)) => from matcher xs replaceAll to
  }

  def isNonEmptyDigits   = xs matches """^[\d]+$"""
  def isAllWhitespace    = xs matches """[\s]*"""
  def nonEmpty: Boolean  = onull.length > 0
  def capitalize: String = mapNonEmpty(s => "" + s.head.toUpper + s.tail)

  def length                      = xs.length
  def * (n: Int): String          = n times xs mkString ""
  def format(args : Any*): String = java.lang.String.format(xs, args map unwrapArg: _*)

  def bytes: Array[Byte]              = xs.getBytes
  def chars: Array[Char]              = xs.toCharArray
  def wordSet: pSet[String]           = words.naturalSet
  def lineVector: pVector[String]     = splitChar('\n')
  def dollarSegments: pVector[String] = splitChar('$')
  def dottedSegments: pVector[String] = splitChar('.')
  def slashSegments: pVector[String]  = splitChar('/')

  def containsChar(ch: Char): Boolean       = chars contains ch
  def splitChar(ch: Char): pVector[String]  = splitRegex(Regex quote ch.toString)
  def splitRegex(r: Regex): pVector[String] = r.pattern split xs pvec
  def words: pVector[String]                = splitRegex(whitespace)

  def mapNonEmpty(f: Unary[String]): String        = if (isEmpty) "" else f(xs)
  def mapSplit(ch: Char)(f: Unary[String]): String = splitChar(ch) map f mkString ch.toString
  def mapLines(f: Unary[String]): String           = mapSplit('\n')(f)
  def mapChars(pf: Char ?=> Char): String          = xs map (c => if (pf isDefinedAt c) pf(c) else c)
  def stripMargin(marginChar: Char): String        = mapLines(_ remove ("""^\s*[""" + marginChar + "]").r)

  def toInt: Int       = foldPrefix("0x")(jl.Integer parseInt xs, jl.Integer.parseInt(_, 16))
  def toLong: Long     = foldPrefix("0x")(jl.Long parseLong xs, jl.Long.parseLong(_, 16))
  def toDouble: Double = jl.Double parseDouble xs
  def toFloat: Float   = jl.Float parseFloat xs

  def readAs[A](implicit reads: Read[A]): A = reads read xs

  def foldPrefix[A](prefix: String)(none: => A, some: String => A): A = if (xs startsWith prefix) some(xs drop prefix.length) else none
  def foldSuffix[A](suffix: String)(none: => A, some: String => A): A = if (xs endsWith suffix) some(xs dropRight suffix.length) else none

  def trimTrailing: String        = mapLines(_ remove whitespace.ends)
  def trimLeading: String         = mapLines(_ remove whitespace.starts)
  def stripAnsi: String           = removeAll(ansiCodes)
  def stripMargin: String         = stripMargin('|')
  def stripPrefix(prefix: String) = xs remove prefix.r.literal.starts
  def stripSuffix(suffix: String) = xs remove suffix.r.literal.ends
  def sanitize: String            = mapChars { case x if x.isControl => '?' }

  def truncateAndLeftJustifyTo(maxlen: Int): String =
    ("%-" + maxlen + "s").format(normalizeSpace truncateTo maxlen)

  def truncateTo(maxlen: Int): String =
    if (length <= maxlen) xs else xs.take(maxlen - 3) + "..."

  def normalizeSpace: String = xs.trim.replacePattern(
    "\\n+"      -> "\n",
    "([{(])\\n" -> "$1 ",
    "\\n"       -> "; ",
    "\\s+"      -> " "
  )

  override def toString = xs
}
