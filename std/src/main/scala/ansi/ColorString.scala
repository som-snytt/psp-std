package psp
package std
package ansi

import api._

/** A String carrying color information.
 *  The length of a ColorString is it's *visible* length, which is
 *  likely significantly smaller than the number of characters it contains.
 */
sealed trait ColorString {
  def raw: String
  def length: Int
  def colorized: String
}

object ColorString {
  implicit val ShowColorString: Show[ColorString] = Show[ColorString](_.colorized)

  def apply(raw: String): ColorString                        = new Atomic(raw, Ansi.empty)
  def apply(raw: String, code: Ansi): ColorString            = new Atomic(raw, code)
  def apply(lhs: ColorString, rhs: ColorString): ColorString = new Join(lhs, rhs)

  final class Atomic(val raw: String, code: Ansi) extends ColorString {
    def length    = raw.length
    def colorized = code(raw)
    override def toString = s"Atom($raw, $code)"
  }
  final class Join(val lhs: ColorString, val rhs: ColorString) extends ColorString {
    def raw: String       = lhs.raw + rhs.raw
    def length: Int       = lhs.length + rhs.length
    def colorized: String = lhs.colorized + rhs.colorized
    override def toString = s"Join($lhs, $rhs)"
  }
}

final class ColoredOps(val lhs: ColorString) extends AnyVal {
  def ~(rhs: ColorString): ColorString = ColorString(lhs, rhs)
}

final class TextOps(val lhs: String) extends AnyVal with BasicAtoms[ColorString] {
  def fg = ansi.fg map (ColorString(lhs, _))
  def bg = ansi.bg map (ColorString(lhs, _))

  protected def newRep(atom: Atom): ColorString = ColorString(lhs, Ansi(atom))

  /** Gave in to one bit of punctuation on String, because everyone adds
   *  strings with '+' and we need something which higher precedence
   *  for it to be at all satisfying.
   *
   *  "foo" %> Red + "bar" %> Magenta.bright
   */
  def %>(code: Ansi): ColorString = ColorString(lhs, code)
}
