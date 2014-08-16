package psp
package core

import java.util.regex.{ Pattern, Matcher, MatchResult }
import psp.std._

final class Regex(val pattern: Pattern) extends AnyVal {
  private def matcher(input: String): Matcher = pattern matcher input

  def matchesWhole(input: String): Boolean    = matcher(input).matches()
  def matchesAnywhere(input: String): Boolean = matcher(input).find()
  def splits(input: String): Direct[String]   = Direct pure (pattern split input)

  def unapplySeq(input: String): Option[Vector[String]] = matcher(input) match {
    case m if m.matches() => Some((1 to m.groupCount).toVector map (i => m.group(i)))
    case _                => None
  }

  def literal: Regex = Regex(Pattern quote pattern.toString)

  override def toString = pattern.toString
}

object Regex {
  def apply(p: Pattern): Regex = new Regex(p)
  def apply(s: String): Regex  = new Regex(Pattern compile s)
}
