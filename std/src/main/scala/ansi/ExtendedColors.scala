package psp
package std
package ansi

trait BasicAtoms[Rep] extends Any with BasicColors[Rep] with BasicAttributes[Rep] {
  protected def newRep(atom: Atom): Rep
  protected def newColor(atom: Atom)     = newRep(atom)
  protected def newAttribute(atom: Atom) = newRep(atom)
}

trait BasicColors[Color] extends Any {
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

trait BasicAttributes[Attribute] extends Any {
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

  protected def create(code: Int): Rep

  implicit private def implicitExtendedColor(code: Int): Rep = create(code)

  def map[S](g: Rep => S): ExtendedColors[S] = new ExtendedColors[S] { protected def create(code: Int): S = g(outer.create(code)) }

  private def base6(n: Int): Int                 = if (n == 0) 0 else if (n == 255) 5 else (n / 51) + 1
  private def rgb6(r: Int, g: Int, b: Int): Int  = printResult(s"rgb6($r, $g, $b)")(base6(r) * 36 + base6(g) * 6 + base6(b))
  protected def rgb(r: Int, g: Int, b: Int): Rep = printResult(s"rgb($r, $g, $b)")(16 + rgb6(r, g, b))

  def colorMap: Map[String, Rep] = Map[String, Rep](
    "aliceblue"            -> aliceblue,
    "antiquewhite"         -> antiquewhite,
    "aqua"                 -> aqua,
    "aquamarine"           -> aquamarine,
    "azure"                -> azure,
    "beige"                -> beige,
    "bisque"               -> bisque,
    "black"                -> black,
    "blanchedalmond"       -> blanchedalmond,
    "blue"                 -> blue,
    "blueviolet"           -> blueviolet,
    "brown"                -> brown,
    "burlywood"            -> burlywood,
    "cadetblue"            -> cadetblue,
    "chartreuse"           -> chartreuse,
    "chocolate"            -> chocolate,
    "coral"                -> coral,
    "cornflowerblue"       -> cornflowerblue,
    "cornsilk"             -> cornsilk,
    "crimson"              -> crimson,
    "cyan"                 -> cyan,
    "darkblue"             -> darkblue,
    "darkcyan"             -> darkcyan,
    "darkgoldenrod"        -> darkgoldenrod,
    "darkgray"             -> darkgray,
    "darkgreen"            -> darkgreen,
    "darkgrey"             -> darkgrey,
    "darkkhaki"            -> darkkhaki,
    "darkmagenta"          -> darkmagenta,
    "darkolivegreen"       -> darkolivegreen,
    "darkorange"           -> darkorange,
    "darkorchid"           -> darkorchid,
    "darkred"              -> darkred,
    "darksalmon"           -> darksalmon,
    "darkseagreen"         -> darkseagreen,
    "darkslateblue"        -> darkslateblue,
    "darkslategray"        -> darkslategray,
    "darkslategrey"        -> darkslategrey,
    "darkturquoise"        -> darkturquoise,
    "darkviolet"           -> darkviolet,
    "deeppink"             -> deeppink,
    "deepskyblue"          -> deepskyblue,
    "dimgray"              -> dimgray,
    "dimgrey"              -> dimgrey,
    "dodgerblue"           -> dodgerblue,
    "firebrick"            -> firebrick,
    "floralwhite"          -> floralwhite,
    "forestgreen"          -> forestgreen,
    "fuchsia"              -> fuchsia,
    "gainsboro"            -> gainsboro,
    "ghostwhite"           -> ghostwhite,
    "gold"                 -> gold,
    "goldenrod"            -> goldenrod,
    "gray"                 -> gray,
    "green"                -> green,
    "greenyellow"          -> greenyellow,
    "grey"                 -> grey,
    "honeydew"             -> honeydew,
    "hotpink"              -> hotpink,
    "indianred"            -> indianred,
    "indigo"               -> indigo,
    "ivory"                -> ivory,
    "khaki"                -> khaki,
    "lavender"             -> lavender,
    "lavenderblush"        -> lavenderblush,
    "lawngreen"            -> lawngreen,
    "lemonchiffon"         -> lemonchiffon,
    "lightblue"            -> lightblue,
    "lightcoral"           -> lightcoral,
    "lightcyan"            -> lightcyan,
    "lightgoldenrodyellow" -> lightgoldenrodyellow,
    "lightgray"            -> lightgray,
    "lightgreen"           -> lightgreen,
    "lightgrey"            -> lightgrey,
    "lightpink"            -> lightpink,
    "lightsalmon"          -> lightsalmon,
    "lightseagreen"        -> lightseagreen,
    "lightskyblue"         -> lightskyblue,
    "lightslategray"       -> lightslategray,
    "lightslategrey"       -> lightslategrey,
    "lightsteelblue"       -> lightsteelblue,
    "lightyellow"          -> lightyellow,
    "lime"                 -> lime,
    "limegreen"            -> limegreen,
    "linen"                -> linen,
    "magenta"              -> magenta,
    "maroon"               -> maroon,
    "mediumaquamarine"     -> mediumaquamarine,
    "mediumblue"           -> mediumblue,
    "mediumorchid"         -> mediumorchid,
    "mediumpurple"         -> mediumpurple,
    "mediumseagreen"       -> mediumseagreen,
    "mediumslateblue"      -> mediumslateblue,
    "mediumspringgreen"    -> mediumspringgreen,
    "mediumturquoise"      -> mediumturquoise,
    "mediumvioletred"      -> mediumvioletred,
    "midnightblue"         -> midnightblue,
    "mintcream"            -> mintcream,
    "mistyrose"            -> mistyrose,
    "moccasin"             -> moccasin,
    "navajowhite"          -> navajowhite,
    "navy"                 -> navy,
    "oldlace"              -> oldlace,
    "olive"                -> olive,
    "olivedrab"            -> olivedrab,
    "orange"               -> orange,
    "orangered"            -> orangered,
    "orchid"               -> orchid,
    "palegoldenrod"        -> palegoldenrod,
    "palegreen"            -> palegreen,
    "paleturquoise"        -> paleturquoise,
    "palevioletred"        -> palevioletred,
    "papayawhip"           -> papayawhip,
    "peachpuff"            -> peachpuff,
    "peru"                 -> peru,
    "pink"                 -> pink,
    "plum"                 -> plum,
    "powderblue"           -> powderblue,
    "purple"               -> purple,
    "red"                  -> red,
    "rosybrown"            -> rosybrown,
    "royalblue"            -> royalblue,
    "saddlebrown"          -> saddlebrown,
    "salmon"               -> salmon,
    "sandybrown"           -> sandybrown,
    "seagreen"             -> seagreen,
    "seashell"             -> seashell,
    "sienna"               -> sienna,
    "silver"               -> silver,
    "skyblue"              -> skyblue,
    "slateblue"            -> slateblue,
    "slategray"            -> slategray,
    "slategrey"            -> slategrey,
    "snow"                 -> snow,
    "springgreen"          -> springgreen,
    "steelblue"            -> steelblue,
    "tan"                  -> tan,
    "teal"                 -> teal,
    "thistle"              -> thistle,
    "tomato"               -> tomato,
    "turquoise"            -> turquoise,
    "violet"               -> violet,
    "wheat"                -> wheat,
    "white"                -> white,
    "whitesmoke"           -> whitesmoke,
    "yellow"               -> yellow,
    "yellowgreen"          -> yellowgreen
  )

  val aliceblue            = rgb ( 240, 248, 255 )
  val antiquewhite         = rgb ( 250, 235, 215 )
  val aqua                 = rgb ( 0  , 255, 255 )
  val aquamarine           = rgb ( 127, 255, 212 )
  val azure                = rgb ( 240, 255, 255 )
  val beige                = rgb ( 245, 245, 220 )
  val bisque               = rgb ( 255, 228, 196 )
  val black                = rgb ( 0  , 0  ,   0 )
  val blanchedalmond       = rgb ( 255, 235, 205 )
  val blue                 = rgb ( 0  , 0  , 255 )
  val blueviolet           = rgb ( 138, 43 , 226 )
  val brown                = rgb ( 165, 42 ,  42 )
  val burlywood            = rgb ( 222, 184, 135 )
  val cadetblue            = rgb ( 95 , 158, 160 )
  val chartreuse           = rgb ( 127, 255,   0 )
  val chocolate            = rgb ( 210, 105,  30 )
  val coral                = rgb ( 255, 127,  80 )
  val cornflowerblue       = rgb ( 100, 149, 237 )
  val cornsilk             = rgb ( 255, 248, 220 )
  val crimson              = rgb ( 220, 20 ,  60 )
  val cyan                 = rgb ( 0  , 255, 255 )
  val darkblue             = rgb ( 0  , 0  , 139 )
  val darkcyan             = rgb ( 0  , 139, 139 )
  val darkgoldenrod        = rgb ( 184, 134,  11 )
  val darkgray             = rgb ( 169, 169, 169 )
  val darkgreen            = rgb ( 0  , 100,   0 )
  val darkgrey             = rgb ( 169, 169, 169 )
  val darkkhaki            = rgb ( 189, 183, 107 )
  val darkmagenta          = rgb ( 139, 0  , 139 )
  val darkolivegreen       = rgb ( 85 , 107,  47 )
  val darkorange           = rgb ( 255, 140,   0 )
  val darkorchid           = rgb ( 153, 50 , 204 )
  val darkred              = rgb ( 139, 0  ,   0 )
  val darksalmon           = rgb ( 233, 150, 122 )
  val darkseagreen         = rgb ( 143, 188, 143 )
  val darkslateblue        = rgb ( 72 , 61 , 139 )
  val darkslategray        = rgb ( 47 , 79 ,  79 )
  val darkslategrey        = rgb ( 47 , 79 ,  79 )
  val darkturquoise        = rgb ( 0  , 206, 209 )
  val darkviolet           = rgb ( 148, 0  , 211 )
  val deeppink             = rgb ( 255, 20 , 147 )
  val deepskyblue          = rgb ( 0  , 191, 255 )
  val dimgray              = rgb ( 105, 105, 105 )
  val dimgrey              = rgb ( 105, 105, 105 )
  val dodgerblue           = rgb ( 30 , 144, 255 )
  val firebrick            = rgb ( 178, 34 ,  34 )
  val floralwhite          = rgb ( 255, 250, 240 )
  val forestgreen          = rgb ( 34 , 139,  34 )
  val fuchsia              = rgb ( 255, 0  , 255 )
  val gainsboro            = rgb ( 220, 220, 220 )
  val ghostwhite           = rgb ( 248, 248, 255 )
  val gold                 = rgb ( 255, 215,   0 )
  val goldenrod            = rgb ( 218, 165,  32 )
  val gray                 = rgb ( 128, 128, 128 )
  val green                = rgb ( 0  , 128,   0 )
  val greenyellow          = rgb ( 173, 255,  47 )
  val grey                 = rgb ( 128, 128, 128 )
  val honeydew             = rgb ( 240, 255, 240 )
  val hotpink              = rgb ( 255, 105, 180 )
  val indianred            = rgb ( 205, 92 ,  92 )
  val indigo               = rgb ( 75 , 0  , 130 )
  val ivory                = rgb ( 255, 255, 240 )
  val khaki                = rgb ( 240, 230, 140 )
  val lavender             = rgb ( 230, 230, 250 )
  val lavenderblush        = rgb ( 255, 240, 245 )
  val lawngreen            = rgb ( 124, 252,   0 )
  val lemonchiffon         = rgb ( 255, 250, 205 )
  val lightblue            = rgb ( 173, 216, 230 )
  val lightcoral           = rgb ( 240, 128, 128 )
  val lightcyan            = rgb ( 224, 255, 255 )
  val lightgoldenrodyellow = rgb ( 250, 250, 210 )
  val lightgray            = rgb ( 211, 211, 211 )
  val lightgreen           = rgb ( 144, 238, 144 )
  val lightgrey            = rgb ( 211, 211, 211 )
  val lightpink            = rgb ( 255, 182, 193 )
  val lightsalmon          = rgb ( 255, 160, 122 )
  val lightseagreen        = rgb ( 32 , 178, 170 )
  val lightskyblue         = rgb ( 135, 206, 250 )
  val lightslategray       = rgb ( 119, 136, 153 )
  val lightslategrey       = rgb ( 119, 136, 153 )
  val lightsteelblue       = rgb ( 176, 196, 222 )
  val lightyellow          = rgb ( 255, 255, 224 )
  val lime                 = rgb ( 0  , 255,   0 )
  val limegreen            = rgb ( 50 , 205,  50 )
  val linen                = rgb ( 250, 240, 230 )
  val magenta              = rgb ( 255, 0  , 255 )
  val maroon               = rgb ( 128, 0  ,   0 )
  val mediumaquamarine     = rgb ( 102, 205, 170 )
  val mediumblue           = rgb ( 0  , 0  , 205 )
  val mediumorchid         = rgb ( 186, 85 , 211 )
  val mediumpurple         = rgb ( 147, 112, 219 )
  val mediumseagreen       = rgb ( 60 , 179, 113 )
  val mediumslateblue      = rgb ( 123, 104, 238 )
  val mediumspringgreen    = rgb ( 0  , 250, 154 )
  val mediumturquoise      = rgb ( 72 , 209, 204 )
  val mediumvioletred      = rgb ( 199, 21 , 133 )
  val midnightblue         = rgb ( 25 , 25 , 112 )
  val mintcream            = rgb ( 245, 255, 250 )
  val mistyrose            = rgb ( 255, 228, 225 )
  val moccasin             = rgb ( 255, 228, 181 )
  val navajowhite          = rgb ( 255, 222, 173 )
  val navy                 = rgb ( 0  , 0  , 128 )
  val oldlace              = rgb ( 253, 245, 230 )
  val olive                = rgb ( 128, 128,   0 )
  val olivedrab            = rgb ( 107, 142,  35 )
  val orange               = rgb ( 255, 165,   0 )
  val orangered            = rgb ( 255, 69 ,   0 )
  val orchid               = rgb ( 218, 112, 214 )
  val palegoldenrod        = rgb ( 238, 232, 170 )
  val palegreen            = rgb ( 152, 251, 152 )
  val paleturquoise        = rgb ( 175, 238, 238 )
  val palevioletred        = rgb ( 219, 112, 147 )
  val papayawhip           = rgb ( 255, 239, 213 )
  val peachpuff            = rgb ( 255, 218, 185 )
  val peru                 = rgb ( 205, 133,  63 )
  val pink                 = rgb ( 255, 192, 203 )
  val plum                 = rgb ( 221, 160, 221 )
  val powderblue           = rgb ( 176, 224, 230 )
  val purple               = rgb ( 128, 0  , 128 )
  val red                  = rgb ( 255, 0  ,   0 )
  val rosybrown            = rgb ( 188, 143, 143 )
  val royalblue            = rgb ( 65 , 105, 225 )
  val saddlebrown          = rgb ( 139, 69 ,  19 )
  val salmon               = rgb ( 250, 128, 114 )
  val sandybrown           = rgb ( 244, 164,  96 )
  val seagreen             = rgb ( 46 , 139,  87 )
  val seashell             = rgb ( 255, 245, 238 )
  val sienna               = rgb ( 160, 82 ,  45 )
  val silver               = rgb ( 192, 192, 192 )
  val skyblue              = rgb ( 135, 206, 235 )
  val slateblue            = rgb ( 106, 90 , 205 )
  val slategray            = rgb ( 112, 128, 144 )
  val slategrey            = rgb ( 112, 128, 144 )
  val snow                 = rgb ( 255, 250, 250 )
  val springgreen          = rgb ( 0  , 255, 127 )
  val steelblue            = rgb ( 70 , 130, 180 )
  val tan                  = rgb ( 210, 180, 140 )
  val teal                 = rgb ( 0  , 128, 128 )
  val thistle              = rgb ( 216, 191, 216 )
  val tomato               = rgb ( 255, 99 ,  71 )
  val turquoise            = rgb ( 64 , 224, 208 )
  val violet               = rgb ( 238, 130, 238 )
  val wheat                = rgb ( 245, 222, 179 )
  val white                = rgb ( 255, 255, 255 )
  val whitesmoke           = rgb ( 245, 245, 245 )
  val yellow               = rgb ( 255, 255,   0 )
  val yellowgreen          = rgb ( 154, 205,  50 )
}
