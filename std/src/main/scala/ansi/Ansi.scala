package psp
package std
package ansi

import Ansi._

object Ansi {
  final val ESC       = '\u001b'                    // <esc>
  final val LBR       = '\u005b'                    // [
  final val CSI       = new String(Array(ESC, LBR)) // control sequence introducer
  final val CSI_FINAL = "m"                         // the last episode of "CSI: Crime Scene Investigation."
  final val SEMI      = ";"
  final val RESET     = apply(0)

  private val StripRegex = """\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]"""

  val empty: Ansi = new Ansi(Vector())

  def apply(atom: Atom, atoms: Atom*): Ansi = new Ansi(atom +: atoms.toVector)
  def apply(codes: Int*): Ansi              = if (codes.isEmpty) empty else apply(Atom(codes.head), codes.tail map Atom: _*)
  def csi(codes: Int*): String              = codes.mkString(CSI, SEMI, CSI_FINAL)
  def strip(s: String): String              = s.replaceAll(StripRegex, "")
}

/** An ansi control sequence. Applying it to a String
 *  produces a String surrounded by the appropriate control
 *  characters.
 */
final class Ansi private (val atoms: Vector[Atom]) extends BasicAttributes[Ansi] with (String => String) {
  type Attribute = Ansi

  protected def newAttribute(atom: Atom): Ansi = Ansi(atom, atoms: _*)

  def /(that: Atom) = new Ansi(atoms :+ that)
  def /(that: Ansi) = new Ansi(atoms ++ that.atoms)

  def apply(s: String): String = "%s%s%s".format(csi(atoms map (_.code): _*), s, csi(0))

  override def toString = atoms mkString ("\\" + "e[", ";", "m")
}
