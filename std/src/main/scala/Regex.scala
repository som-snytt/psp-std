package psp
package std

import java.util.regex.Pattern, Pattern._
import java.util.regex.Matcher
import java.util.regex.PatternSyntaxException
import api._

// By default, the regular expressions ^ and $ ignore line terminators and only match at the beginning and the end, respectively, of the entire input sequence.
// If MULTILINE mode is activated then ^ matches at the beginning of input and after any line terminator except at the end of input.
// When in MULTILINE mode $ matches just before a line terminator or the end of the input sequence.
//
// The regular expression . matches any character except a line terminator unless the DOTALL flag is specified.
final class Regex(val pattern: Pattern) extends AnyVal {
  def matcher(input: CharSequence): Matcher = pattern matcher input useAnchoringBounds false
  def to_s = pattern.toString

  def flags: Int        = pattern.flags
  def multiline         = setFlag(MULTILINE)
  def caseInsensitive   = setFlag(CASE_INSENSITIVE)
  def literal           = mapRegex("\\Q" + _ + "\\E") // Not setFlag(LITERAL) lest further regex additions be misinterpreted
  def dotall            = setFlag(DOTALL)
  def comments          = setFlag(COMMENTS)
  def starts            = mapRegex("^" + _)
  def ends              = mapRegex(_ + "$")
  def anchors           = mapRegex("^" + _ + "$")
  def noAnchors         = mapRegex(_ stripPrefix "^" stripSuffix "$")
  def zeroOrMore        = mapRegex(_ + "*")
  def oneOrMore         = mapRegex(_ + "+")
  def characterClass    = mapRegex("[" + _ + "]")
  def capturingGroup    = mapRegex("(" + _ + ")")
  def nonCapturingGroup = mapRegex("(?:" + _ + ")")

  def |(that: Regex): Regex = mapRegex(_ + "|" + that.pattern)
  def ~(that: Regex): Regex = mapRegex(_ + that.pattern)

  def setFlag(flag: Int): Regex         = Regex(to_s, flags | flag)
  def mapRegex(f: Unary[String]): Regex = Regex(f(to_s), flags)

  def isMatch[A: Show](x: A): Boolean       = isMatch(x.to_s)
  def isMatch(input: CharSequence): Boolean = matcher(input).matches
  def all(input: CharSequence)              = matcher(input) |> (m => option(m.matches(), 1 to m.groupCount map m.group))
  def first(input: CharSequence)            = matcher(input) |> (m => option(m.find, m.group()))
  def unapplySeq(input: CharSequence)       = all(input) map (_.seq)

  override def toString = s"$pattern"
}

object Regex extends (String => Regex) {
  def whitespace = apply("""[\s]+""")
  def newlines   = apply("""[\n]+""")
  def ansiCodes  = apply("""\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]""")

  implicit def regexPredicate(r: Regex): Predicate[String] = r.isMatch

  def maybe(s: String): Option[Regex]     = try Some(apply(s)) catch { case _: PatternSyntaxException => None }
  def quote(s: String): Regex             = apply(Pattern quote s)
  def apply(s: String): Regex             = new Regex(Pattern compile s)
  def apply(s: String, flags: Int): Regex = new Regex(Pattern.compile(s, flags))
}

final class SplitString(s: String, where: Index) extends scala.Product2[String, String] {
  def isEmpty             = where == NoIndex
  def get                 = (_1, _2)
  def _1                  = s.substring(0, where.safeToInt)
  def _2                  = s.substring(where.safeToInt, s.length)
  def canEqual(that: Any) = that.isInstanceOf[SplitString]
}

object LiteralReplacement {
  implicit def liftPair(x: (String, String)): LiteralReplacement = LiteralReplacement(x._1, x._2)
}
object PatternReplacement {
  implicit def liftPair(x: (String, String)): PatternReplacement = PatternReplacement(x._1.r, x._2)
}

final case class LiteralReplacement(from: String, to: String)
final case class PatternReplacement(from: Regex, to: String)
