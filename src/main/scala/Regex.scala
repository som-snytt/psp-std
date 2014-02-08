package psp
package core

import java.util.regex.{ Pattern, Matcher, MatchResult }

final class MyResult {
  def end(x$1: Int): Int = ???
  def end(): Int = ???
  def group(x$1: Int): String = ???
  def group(): String = ???
  def groupCount(): Int = ???
  def start(x$1: Int): Int = ???
  def start(): Int = ???
}

final class Regex(val pattern: Pattern) extends AnyVal {
  private def matcher(input: CharSequence): Matcher = pattern matcher input

  // def allIn(input: CharSequence): Direct[String]   =
  def matchesWhole(input: CharSequence): Boolean    = matcher(input).matches()
  def matchesAnywhere(input: CharSequence): Boolean = matcher(input).find()
  def splits(input: CharSequence): Direct[String]  = Direct pure (pattern split input)

  def unapplySeq(input: CharSequence): Option[List[String]] = matcher(input) match {
    case m if m.matches() => Some((1 to m.groupCount).toList map m.group)
    case _                => None
  }

  def literal: Regex = Regex(Pattern quote pattern.toString)

  override def toString = pattern.toString
}

object Regex {
  def apply(p: Pattern): Regex = new Regex(p)
  def apply(s: String): Regex  = new Regex(Pattern compile s)
}
