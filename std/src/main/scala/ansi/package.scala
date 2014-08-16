package psp
package std

package object ansi extends BasicAtoms[Ansi] {
  protected def newRep(atom: Atom): Ansi = Ansi(atom)

  object fg extends ExtendedColors[Ansi] {
    def create(name: String, rgb: RGB): Ansi = (Atom(38) +: Atom(5) +: rgb.colorAtoms).foldLeft(Ansi.empty)(_ / _)
  }
  object bg extends ExtendedColors[Ansi] {
    def create(name: String, rgb: RGB): Ansi = (Atom(48) +: Atom(5) +: rgb.colorAtoms).foldLeft(Ansi.empty)(_ / _)
  }

  private def actualLines(resource: String) = resourceString(resource).lines filterNot (_ startsWith "#")

  lazy val colorMap: RgbMap = new RgbMap(
    actualLines("xkcd-rgb-tabular.txt") mapToMapPairs { s =>
      val Vector(name, r, g, b) = s.words
      ColorName(name) -> RGB(r.toInt, g.toInt, b.toInt)
    },
    actualLines("xterm256-rgb.txt") map (s => RGB fromBits Integer.parseInt(s.words.last, 16))
  )

  implicit def impliciStringOps(x: String): TextOps               = new TextOps(x)
  implicit def implicitColorStringOps(x: ColorString): ColoredOps = new ColoredOps(x)
  implicit def implicitLiftAnsiAtom(c: Atom): Ansi                = Ansi(c)
}
