package psp
package std
package ansi

import RGB._

final case class ColorName(name: String)

object ColorName {
  def empty = ColorName("<none>")

  implicit def ReadColorNames = Read[Seq[ColorName]](s => colorMap namesOf s.to[RGB])
  implicit def ShowColorName  = Show[ColorName](x => (colorMap get x).fold(x.name)(idx => Ansi(38, 5, idx.value)(x.name)))
}

final class RGB private (val bits: Int) extends AnyVal {
  def hex: String = bits.hex
  def hexColor    = hexN(this.redValue, 2).red ~ hexN(this.greenValue, 2).green ~ hexN(this.blueValue, 2).blue
  override def toString = s"RGB($bits)"
}

final class RgbMap(val colors: OrderedMap[ColorName, RGB], val palette: Vector[RGB]) {
  def grouped = (colors.keys groupBy nearestIndex).values.toVector sortBy (x => (x.length, x.head.name.length)) map (_.to_s)
  def showMap = colors.keys mapToMapPairs { k =>
    val direct = colors(k).hexColor.to_s
    val index = nearestIndex(colors(k))
    val indirect = nearest(colors(k)).hexColor.to_s
    val distance = colors(k) distanceTo nearest(colors(k))

    k -> "#%6s => %3s %s (%.3f)".format(direct, index, indirect, distance)
  }

  def get(key: ColorName): Option[Index]  = Try(nearestIndex(key)).toOption
  def namesOf(rgb: RGB): Seq[ColorName]   = colors.keys filter (k => nearest(rgb) == nearest(colors(k)))
  def nearestIndex(key: ColorName): Index = nearestIndex(colors(key))
  def nearest(rgb: RGB): RGB              = palette minBy rgb.distanceTo
  def nearestIndex(rgb: RGB): Index       = Index(palette.indices minBy (i => rgb distanceTo palette(i)))

  override def toString = showMap.to_s
}

object RGB {
  def fromBits(bits: Int): RGB = apply((bits >> 16) & 0xFF, (bits >> 8) & 0xFF, bits & 0xFF)

  def hexN(value: Int, digits: Int): String = value.hex |> (s => "%s%s".format("0" * (digits - s.length), s))

  implicit def ShowRGB = Show[RGB](x => (hexN(x.redValue, 2).red ~ hexN(x.greenValue, 2).green ~ hexN(x.blueValue, 2).blue).to_s)

  def ReadRGBHex1: Read[RGB] = Read[RGB](s => fromBits(("0x" + s).toInt))
  def ReadRGBDec3: Read[RGB] = Read[RGB](s => s.words match { case Vector(r, g, b) => RGB(r.toInt, g.toInt, b.toInt) })

  implicit def ReadRGB: Read[RGB] = Read[RGB](s => if (s.words.length == 1) ReadRGBHex1 read s else ReadRGBDec3 read s)

  implicit class RGBOps(val rgb: RGB) {
    def redValue    = (rgb.bits >> 16) & 0xFF
    def greenValue  = (rgb.bits >>  8) & 0xFF
    def blueValue   = (rgb.bits >>  0) & 0xFF
    def colorValues = Vector(redValue, greenValue, blueValue)
    def colorAtoms  = colorValues map Atom
    def distanceTo(that: RGB): Double = {
      val squares = colorValues zip that.colorValues map { case (x, y) => math.pow((x - y).abs, 2) }
      math.sqrt(squares.sum)
    }
  }
  private def inRange(x: Int): Unit = require(0 <= x && x < 256, s"0 <= x && x < 256")
  def apply(r: Int, g: Int, b: Int): RGB = {
    inRange(r) ; inRange(g) ; inRange(b)
    new RGB((r << 16) | (g << 8) | b)
  }

}
