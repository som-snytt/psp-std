package psp
package std
package ansi

import Ansi._, StdShow._

object Ansi {
  final val ESC       = '\u001b'                    // <esc>
  final val LBR       = '\u005b'                    // [
  final val CSI       = new String(Array(ESC, LBR)) // control sequence introducer
  final val CSI_FINAL = "m"                         // the last episode of "CSI: Crime Scene Investigation."
  final val SEMI      = ";"
  final val RESET     = apply(0)

  val empty: Ansi = new Ansi(sciVector())

  def apply(atom: Atom, atoms: Atom*): Ansi = new Ansi(atom +: atoms.toVector)
  def apply(codes: Int*): Ansi              = if (codes.isEmpty) empty else apply(Atom(codes.head), codes.tail map Atom: _*)
  def csi(codes: Int*): String              = codes.mkString(CSI, SEMI, CSI_FINAL)
}

/** An ansi control sequence. Applying it to a String
 *  produces a String surrounded by the appropriate control
 *  characters.
 */
final class Ansi private (val atoms: sciVector[Atom]) extends BasicAttributes[Ansi] with (String => String) {
  type Attribute = Ansi

  protected def newAttribute(atom: Atom): Ansi = Ansi(atom, atoms: _*)

  def /(that: Atom) = new Ansi(atoms :+ that)
  def /(that: Ansi) = new Ansi(atoms ++ that.atoms)

  private def prefix = csi(atoms map (_.code): _*)
  private def suffix = csi(0)

  def apply(s: String): String = prefix ~ s ~ suffix

  override def toString = atoms mk_s ";"
}

/** One piece of an ansi control sequence.  Either a color
 *  (foreground or background) or an attribute (e.g. bright, underline.)
 *  Control sequences are created from Atoms with the / operator.
 */
final class Atom private (val value: Int) extends AnyVal with ForceShowDirect {
  def code: Int = value
  def to_s      = s"$code"
}

object Atom extends (Int => Atom) {
  def apply(n: Int): Atom = if (n < 0) illegalArgumentException(s"$n") else new Atom(n)
}
