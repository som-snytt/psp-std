package psp
package std
package ansi

import api._
import RGB._
import StdShow._
import StdEq._

final case class ColorName(name: String)

object ColorName {
  def empty = ColorName("<none>")

  implicit def ReadColorNames: Read[pVector[ColorName]] = Read[pVector[ColorName]](s => colorMap namesOf s.readAs[RGB])
  implicit def ShowColorName: Show[ColorName]           = Show[ColorName](x => (colorMap get x).fold(x.name)(idx => Ansi(38, 5, idx.safeToInt)(x.name)))
}

final class RGB private (val bits: Int) extends AnyVal {
  def hex_s: ColorString = hexN(this.redValue, 2).red ~ hexN(this.greenValue, 2).green ~ hexN(this.blueValue, 2).blue
  override def toString  = s"RGB($bits)"
}

final class RgbMap(val keys: pVector[ColorName], val lookup: ColorName => RGB, val palette: pVector[RGB]) {
  def grouped = (keys.toScalaVector groupBy nearestIndex).values.map(_.pvec) sortOrder (x => (x.length, x.head.name.length)) map (_.joinWords)

  def get(key: ColorName): Option[Index]    = Try(nearestIndex(key)).toOption
  def namesOf(rgb: RGB): pVector[ColorName] = keys filter (k => nearest(rgb) == nearest(lookup(k)))
  def nearestIndex(key: ColorName): Index   = nearestIndex(lookup(key))
  def nearest(rgb: RGB): RGB                = palette.toScalaVector minBy rgb.distanceTo
  def nearestIndex(rgb: RGB): Index         = palette.indices.toScalaVector minBy (i => rgb distanceTo palette(i))

  override def toString = "RgbMap(%s color names, _, %s in palette)".format(keys.length, palette.length)
}

object RgbMap {
  implicit def ShowRgbMap: Show[RgbMap] = Show[RgbMap] { x =>
    import x._
    val pairs = palette.indices map { idx =>
      val rgb = palette(idx)
      def dist(key: ColorName) = lookup(key) distanceTo rgb
      val k = "%3s %s".format(idx, rgb.hex_s.to_s)
      val v = keys.toScalaVector sortBy dist take 5 map { k =>
        val s = Ansi(38, 5, idx.safeToInt)(k.name)
        "%8s  %s%s".format("%.3f" format dist(k), s, " " * (20 - k.name.length))
      }
      k -> (v mkString "  ")
    }
    newMap(pairs).to_s
  }

  def ShowRgbMap2: Show[RgbMap] = Show[RgbMap] { x =>
    import x._
    val pairs = keys sortOrder nearestIndex map { k =>
      val v1       = lookup(k)
      val v2       = nearest(v1)
      val distance = "%.3f" format (v1 distanceTo v2)
      val index    = "%3s" format (palette.toScalaVector indexOf v2)

      k -> show"#$v1 => $index $v2 ($distance)"
    }
    newMap(pairs).to_s
  }
}

object RGB {
  def fromBits(bits: Int): RGB = apply((bits >> 16) & 0xFF, (bits >> 8) & 0xFF, bits & 0xFF)

  def hexN(value: Int, digits: Int): String = value.hex |> (s => "%s%s".format("0" * (digits - s.length), s))

  implicit def ShowRGB: Show[RGB] = Show[RGB](_.hex_s.to_s)
  implicit def ReadRGB: Read[RGB] = Read[RGB](s => if (s.words.length == 1) ReadRGBHex1 read s else ReadRGBDec3 read s)

  def ReadRGBHex1: Read[RGB] = Read[RGB](s => fromBits(("0x" + s).toInt))
  def ReadRGBDec3: Read[RGB] = Read[RGB](s => s.words.toScalaVector match { case Vector(r, g, b) => RGB(r.toInt, g.toInt, b.toInt) })

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
  private def inRange(x: Int): Unit = require(0 <= x && x < 256, "0 <= x && x < 256")
  def apply(r: Int, g: Int, b: Int): RGB = {
    inRange(r) ; inRange(g) ; inRange(b)
    new RGB((r << 16) | (g << 8) | b)
  }

}
