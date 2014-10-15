package psp
package std
package ansi

trait BasicAtoms[Rep] extends scala.Any with BasicColors[Rep] with BasicAttributes[Rep] {
  protected def newRep(atom: Atom): Rep
  protected def newColor(atom: Atom)     = newRep(atom)
  protected def newAttribute(atom: Atom) = newRep(atom)
}

trait BasicColors[Color] extends scala.Any {
  /** This setup allows the color structure to be reused for quite
   *  different purposes. Supply an Atom => Color and you'll have
   *  an object loaded with named Colors.
   */
  protected def newColor(atom: Atom): Color

  /** The lever which turns all these Ints into Colors, whatever that
   *  may mean.
   */
  implicit private def implicitColor(code: Int): Color = newColor(Atom(code))

  def black: Color    = 30
  def red: Color      = 31
  def green: Color    = 32
  def yellow: Color   = 33
  def blue: Color     = 34
  def magenta: Color  = 35
  def cyan: Color     = 36
  def white: Color    = 37
}

trait BasicAttributes[Attribute] extends scala.Any {
  protected def newAttribute(atom: Atom): Attribute
  implicit private def implicitAttribute(code: Int): Attribute = newAttribute(Atom(code))

  def reset: Attribute         = 0
  def bright: Attribute        = 1
  def faint: Attribute         = 2
  def italic: Attribute        = 3
  def underline: Attribute     = 4
  def blink: Attribute         = 5
  def inverse: Attribute       = 7
  def hidden: Attribute        = 8
  def strikethrough: Attribute = 9

  def bold: Attribute    = bright
  def reverse: Attribute = inverse
}

abstract class ExtendedColors[Rep] {
  outer =>

  protected def create(name: String, rgb: RGB): Rep

  def map[S](g: Rep => S): ExtendedColors[S] = new ExtendedColors[S] {
    protected def create(name: String, rgb: RGB): S = g(outer.create(name, rgb))
  }

  private def rgb(r: Int, g: Int, b: Int): RGB = RGB(r, g, b)
  private implicit class As(rgb: RGB) {
    def as(name: String): Rep = create(name, rgb)
  }

  val aliceblue            = rgb ( 240, 248, 255 ) as "aliceblue"
  val antiquewhite         = rgb ( 250, 235, 215 ) as "antiquewhite"
  val aqua                 = rgb ( 0  , 255, 255 ) as "aqua"
  val aquamarine           = rgb ( 127, 255, 212 ) as "aquamarine"
  val azure                = rgb ( 240, 255, 255 ) as "azure"
  val beige                = rgb ( 245, 245, 220 ) as "beige"
  val bisque               = rgb ( 255, 228, 196 ) as "bisque"
  val black                = rgb ( 0  , 0  ,   0 ) as "black"
  val blanchedalmond       = rgb ( 255, 235, 205 ) as "blanchedalmond"
  val blue                 = rgb ( 0  , 0  , 255 ) as "blue"
  val blueviolet           = rgb ( 138, 43 , 226 ) as "blueviolet"
  val brown                = rgb ( 165, 42 ,  42 ) as "brown"
  val burlywood            = rgb ( 222, 184, 135 ) as "burlywood"
  val cadetblue            = rgb ( 95 , 158, 160 ) as "cadetblue"
  val chartreuse           = rgb ( 127, 255,   0 ) as "chartreuse"
  val chocolate            = rgb ( 210, 105,  30 ) as "chocolate"
  val coral                = rgb ( 255, 127,  80 ) as "coral"
  val cornflowerblue       = rgb ( 100, 149, 237 ) as "cornflowerblue"
  val cornsilk             = rgb ( 255, 248, 220 ) as "cornsilk"
  val crimson              = rgb ( 220, 20 ,  60 ) as "crimson"
  val cyan                 = rgb ( 0  , 255, 255 ) as "cyan"
  val darkblue             = rgb ( 0  , 0  , 139 ) as "darkblue"
  val darkcyan             = rgb ( 0  , 139, 139 ) as "darkcyan"
  val darkgoldenrod        = rgb ( 184, 134,  11 ) as "darkgoldenrod"
  val darkgray             = rgb ( 169, 169, 169 ) as "darkgray"
  val darkgreen            = rgb ( 0  , 100,   0 ) as "darkgreen"
  val darkgrey             = rgb ( 169, 169, 169 ) as "darkgrey"
  val darkkhaki            = rgb ( 189, 183, 107 ) as "darkkhaki"
  val darkmagenta          = rgb ( 139, 0  , 139 ) as "darkmagenta"
  val darkolivegreen       = rgb ( 85 , 107,  47 ) as "darkolivegreen"
  val darkorange           = rgb ( 255, 140,   0 ) as "darkorange"
  val darkorchid           = rgb ( 153, 50 , 204 ) as "darkorchid"
  val darkred              = rgb ( 139, 0  ,   0 ) as "darkred"
  val darksalmon           = rgb ( 233, 150, 122 ) as "darksalmon"
  val darkseagreen         = rgb ( 143, 188, 143 ) as "darkseagreen"
  val darkslateblue        = rgb ( 72 , 61 , 139 ) as "darkslateblue"
  val darkslategray        = rgb ( 47 , 79 ,  79 ) as "darkslategray"
  val darkslategrey        = rgb ( 47 , 79 ,  79 ) as "darkslategrey"
  val darkturquoise        = rgb ( 0  , 206, 209 ) as "darkturquoise"
  val darkviolet           = rgb ( 148, 0  , 211 ) as "darkviolet"
  val deeppink             = rgb ( 255, 20 , 147 ) as "deeppink"
  val deepskyblue          = rgb ( 0  , 191, 255 ) as "deepskyblue"
  val dimgray              = rgb ( 105, 105, 105 ) as "dimgray"
  val dimgrey              = rgb ( 105, 105, 105 ) as "dimgrey"
  val dodgerblue           = rgb ( 30 , 144, 255 ) as "dodgerblue"
  val firebrick            = rgb ( 178, 34 ,  34 ) as "firebrick"
  val floralwhite          = rgb ( 255, 250, 240 ) as "floralwhite"
  val forestgreen          = rgb ( 34 , 139,  34 ) as "forestgreen"
  val fuchsia              = rgb ( 255, 0  , 255 ) as "fuchsia"
  val gainsboro            = rgb ( 220, 220, 220 ) as "gainsboro"
  val ghostwhite           = rgb ( 248, 248, 255 ) as "ghostwhite"
  val gold                 = rgb ( 255, 215,   0 ) as "gold"
  val goldenrod            = rgb ( 218, 165,  32 ) as "goldenrod"
  val gray                 = rgb ( 128, 128, 128 ) as "gray"
  val green                = rgb ( 0  , 128,   0 ) as "green"
  val greenyellow          = rgb ( 173, 255,  47 ) as "greenyellow"
  val grey                 = rgb ( 128, 128, 128 ) as "grey"
  val honeydew             = rgb ( 240, 255, 240 ) as "honeydew"
  val hotpink              = rgb ( 255, 105, 180 ) as "hotpink"
  val indianred            = rgb ( 205, 92 ,  92 ) as "indianred"
  val indigo               = rgb ( 75 , 0  , 130 ) as "indigo"
  val ivory                = rgb ( 255, 255, 240 ) as "ivory"
  val khaki                = rgb ( 240, 230, 140 ) as "khaki"
  val lavender             = rgb ( 230, 230, 250 ) as "lavender"
  val lavenderblush        = rgb ( 255, 240, 245 ) as "lavenderblush"
  val lawngreen            = rgb ( 124, 252,   0 ) as "lawngreen"
  val lemonchiffon         = rgb ( 255, 250, 205 ) as "lemonchiffon"
  val lightblue            = rgb ( 173, 216, 230 ) as "lightblue"
  val lightcoral           = rgb ( 240, 128, 128 ) as "lightcoral"
  val lightcyan            = rgb ( 224, 255, 255 ) as "lightcyan"
  val lightgoldenrodyellow = rgb ( 250, 250, 210 ) as "lightgoldenrodyellow"
  val lightgray            = rgb ( 211, 211, 211 ) as "lightgray"
  val lightgreen           = rgb ( 144, 238, 144 ) as "lightgreen"
  val lightgrey            = rgb ( 211, 211, 211 ) as "lightgrey"
  val lightpink            = rgb ( 255, 182, 193 ) as "lightpink"
  val lightsalmon          = rgb ( 255, 160, 122 ) as "lightsalmon"
  val lightseagreen        = rgb ( 32 , 178, 170 ) as "lightseagreen"
  val lightskyblue         = rgb ( 135, 206, 250 ) as "lightskyblue"
  val lightslategray       = rgb ( 119, 136, 153 ) as "lightslategray"
  val lightslategrey       = rgb ( 119, 136, 153 ) as "lightslategrey"
  val lightsteelblue       = rgb ( 176, 196, 222 ) as "lightsteelblue"
  val lightyellow          = rgb ( 255, 255, 224 ) as "lightyellow"
  val lime                 = rgb ( 0  , 255,   0 ) as "lime"
  val limegreen            = rgb ( 50 , 205,  50 ) as "limegreen"
  val linen                = rgb ( 250, 240, 230 ) as "linen"
  val magenta              = rgb ( 255, 0  , 255 ) as "magenta"
  val maroon               = rgb ( 128, 0  ,   0 ) as "maroon"
  val mediumaquamarine     = rgb ( 102, 205, 170 ) as "mediumaquamarine"
  val mediumblue           = rgb ( 0  , 0  , 205 ) as "mediumblue"
  val mediumorchid         = rgb ( 186, 85 , 211 ) as "mediumorchid"
  val mediumpurple         = rgb ( 147, 112, 219 ) as "mediumpurple"
  val mediumseagreen       = rgb ( 60 , 179, 113 ) as "mediumseagreen"
  val mediumslateblue      = rgb ( 123, 104, 238 ) as "mediumslateblue"
  val mediumspringgreen    = rgb ( 0  , 250, 154 ) as "mediumspringgreen"
  val mediumturquoise      = rgb ( 72 , 209, 204 ) as "mediumturquoise"
  val mediumvioletred      = rgb ( 199, 21 , 133 ) as "mediumvioletred"
  val midnightblue         = rgb ( 25 , 25 , 112 ) as "midnightblue"
  val mintcream            = rgb ( 245, 255, 250 ) as "mintcream"
  val mistyrose            = rgb ( 255, 228, 225 ) as "mistyrose"
  val moccasin             = rgb ( 255, 228, 181 ) as "moccasin"
  val navajowhite          = rgb ( 255, 222, 173 ) as "navajowhite"
  val navy                 = rgb ( 0  , 0  , 128 ) as "navy"
  val oldlace              = rgb ( 253, 245, 230 ) as "oldlace"
  val olive                = rgb ( 128, 128,   0 ) as "olive"
  val olivedrab            = rgb ( 107, 142,  35 ) as "olivedrab"
  val orange               = rgb ( 255, 165,   0 ) as "orange"
  val orangered            = rgb ( 255, 69 ,   0 ) as "orangered"
  val orchid               = rgb ( 218, 112, 214 ) as "orchid"
  val palegoldenrod        = rgb ( 238, 232, 170 ) as "palegoldenrod"
  val palegreen            = rgb ( 152, 251, 152 ) as "palegreen"
  val paleturquoise        = rgb ( 175, 238, 238 ) as "paleturquoise"
  val palevioletred        = rgb ( 219, 112, 147 ) as "palevioletred"
  val papayawhip           = rgb ( 255, 239, 213 ) as "papayawhip"
  val peachpuff            = rgb ( 255, 218, 185 ) as "peachpuff"
  val peru                 = rgb ( 205, 133,  63 ) as "peru"
  val pink                 = rgb ( 255, 192, 203 ) as "pink"
  val plum                 = rgb ( 221, 160, 221 ) as "plum"
  val powderblue           = rgb ( 176, 224, 230 ) as "powderblue"
  val purple               = rgb ( 128, 0  , 128 ) as "purple"
  val red                  = rgb ( 255, 0  ,   0 ) as "red"
  val rosybrown            = rgb ( 188, 143, 143 ) as "rosybrown"
  val royalblue            = rgb ( 65 , 105, 225 ) as "royalblue"
  val saddlebrown          = rgb ( 139, 69 ,  19 ) as "saddlebrown"
  val salmon               = rgb ( 250, 128, 114 ) as "salmon"
  val sandybrown           = rgb ( 244, 164,  96 ) as "sandybrown"
  val seagreen             = rgb ( 46 , 139,  87 ) as "seagreen"
  val seashell             = rgb ( 255, 245, 238 ) as "seashell"
  val sienna               = rgb ( 160, 82 ,  45 ) as "sienna"
  val silver               = rgb ( 192, 192, 192 ) as "silver"
  val skyblue              = rgb ( 135, 206, 235 ) as "skyblue"
  val slateblue            = rgb ( 106, 90 , 205 ) as "slateblue"
  val slategray            = rgb ( 112, 128, 144 ) as "slategray"
  val slategrey            = rgb ( 112, 128, 144 ) as "slategrey"
  val snow                 = rgb ( 255, 250, 250 ) as "snow"
  val springgreen          = rgb ( 0  , 255, 127 ) as "springgreen"
  val steelblue            = rgb ( 70 , 130, 180 ) as "steelblue"
  val tan                  = rgb ( 210, 180, 140 ) as "tan"
  val teal                 = rgb ( 0  , 128, 128 ) as "teal"
  val thistle              = rgb ( 216, 191, 216 ) as "thistle"
  val tomato               = rgb ( 255, 99 ,  71 ) as "tomato"
  val turquoise            = rgb ( 64 , 224, 208 ) as "turquoise"
  val violet               = rgb ( 238, 130, 238 ) as "violet"
  val wheat                = rgb ( 245, 222, 179 ) as "wheat"
  val white                = rgb ( 255, 255, 255 ) as "white"
  val whitesmoke           = rgb ( 245, 245, 245 ) as "whitesmoke"
  val yellow               = rgb ( 255, 255,   0 ) as "yellow"
  val yellowgreen          = rgb ( 154, 205,  50 ) as "yellowgreen"
}
