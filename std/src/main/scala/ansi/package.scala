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

  lazy val colorMap: RgbMap = {
    val map = actualLines("xkcd-colors.txt") mapToMapPairs { s =>
      val Vector(name, r, g, b) = s.words
      ColorName(name) -> RGB(r.toInt, g.toInt, b.toInt)
    }
    val palette = actualLines("xterm256-colors.txt") map (_.words.last) map (_.to[RGB])
    new RgbMap(map.keys.toVector, map, palette)
  }

  implicit def impliciStringOps(x: String): TextOps               = new TextOps(x)
  implicit def implicitColorStringOps(x: ColorString): ColoredOps = new ColoredOps(x)
  implicit def implicitLiftAnsiAtom(c: Atom): Ansi                = Ansi(c)
}
