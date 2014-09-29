package psp
package std

import java.util.regex.Pattern, Pattern._
import java.util.regex.Matcher

// By default, the regular expressions ^ and $ ignore line terminators and only match at the beginning and the end, respectively, of the entire input sequence.
// If MULTILINE mode is activated then ^ matches at the beginning of input and after any line terminator except at the end of input.
// When in MULTILINE mode $ matches just before a line terminator or the end of the input sequence.
//
// The regular expression . matches any character except a line terminator unless the DOTALL flag is specified.
final class Regex(val pattern: Pattern) extends AnyVal with ShowDirectNow {
  def matcher(input: String) = pattern matcher input
  def to_s = pattern.toString

  def flags: Int         = pattern.flags
  def setFlag(flag: Int) = Regex(to_s, flags | flag)
  def multiline          = setFlag(MULTILINE)
  def caseInsensitive    = setFlag(CASE_INSENSITIVE)
  def literal            = setFlag(LITERAL)
  def dotall             = setFlag(DOTALL)
  def comments           = setFlag(COMMENTS)

  def mapRegex(f: String => String): Regex = Regex(f(to_s), flags)
  def noAnchors = mapRegex(_ stripPrefix "^" stripSuffix "$")

  def isMatch(input: String) = matcher(input).matches
  def all(input: String) = {
    def loop(m: Matcher): List[String] = if (!m.find) Nil else m.group() :: loop(m)
    loop(matcher(input))
  }
  def first(input: String): Option[String] = {
    val m = matcher(input)
    if (m.find) Some(m.group()) else None
  }
}

object Regex extends (String => Regex) {
  def quote(s: String)                    = Pattern quote s
  def apply(s: String): Regex             = new Regex(Pattern compile s)
  def apply(s: String, flags: Int): Regex = new Regex(Pattern.compile(s, flags))
}
