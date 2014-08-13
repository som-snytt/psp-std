package psp
package std

package object ansi extends BasicAtoms[Ansi] {
  protected def newRep(atom: Atom): Ansi = Ansi(atom)

  object fg extends ExtendedColors[Ansi] {
    def colors256 = Range(1, 256) map create
    def create(code: Int) = Atom(38) / Atom(5) / Atom(code)
    override def rgb(red: Int, green: Int, blue: Int): Ansi = Atom(38) / Atom(5) / super.rgb(red, green, blue)
  }
  object bg extends ExtendedColors[Ansi] {
    def colors256 = Range(1, 256) map create
    def create(code: Int) = Atom(48) / Atom(5) / Atom(code)
  }

  implicit def impliciStringOps(x: String): TextOps               = new TextOps(x)
  implicit def implicitColorStringOps(x: ColorString): ColoredOps = new ColoredOps(x)
  implicit def implicitLiftAnsiAtom(c: Atom): Ansi                = Ansi(c)
}
