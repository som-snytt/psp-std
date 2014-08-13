package psp
package std
package ansi

import Ansi._

/** One piece of an ansi control sequence.  Either a color
 *  (foreground or background) or an attribute (e.g. bright, underline.)
 *  Control sequences are created from Atoms with the / operator.
 */
final class Atom private (val code: Int) extends AnyVal with Ordered[Atom] {
  def compare(that: Atom): Int = code compare that.code
  override def toString = s"$code"
}

object Atom extends (Int => Atom) {
  def apply(n: Int): Atom = if (n < 0) throw new IllegalArgumentException(s"$n") else new Atom(n)
}
