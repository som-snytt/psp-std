package psp
package std

import api._

package object ansi extends BasicAtoms[Ansi] {
  protected def newRep(atom: Atom): Ansi = Ansi(atom)

  object fg extends ExtendedColors[Ansi] {
    def create(name: String, rgb: RGB): Ansi = (Atom(38) +: Atom(5) +: rgb.colorAtoms).foldLeft(Ansi.empty)(_ / _)
  }
  object bg extends ExtendedColors[Ansi] {
    def create(name: String, rgb: RGB): Ansi = (Atom(48) +: Atom(5) +: rgb.colorAtoms).foldLeft(Ansi.empty)(_ / _)
  }

  private def actualLines(resource: String) = resourceString(resource).lineVector filterNot "^#".r

  private def parseLine(line: String) = line.words match {
    case Each(name, r, g, b) => ColorName(name) -> RGB(r.toInt, g.toInt, b.toInt)
  }

  lazy val colorMap: RgbMap = {
    val map     = actualLines("xkcd-colors.txt") map parseLine toExMap
    val palette = actualLines("xterm256-colors.txt") map (_.words.last) map (_.readAs[RGB]) toDirect;
    new RgbMap(map.keyVector, x => map(x), palette)
  }

  implicit def colorNameEq: HashEq[ColorName]                     = HashEq.natural()
  implicit def implicitStringOps(x: String): TextOps              = new TextOps(x)
  implicit def implicitColorStringOps(x: ColorString): ColoredOps = new ColoredOps(x)
  implicit def implicitLiftAnsiAtom(c: Atom): Ansi                = Ansi(c)
}
