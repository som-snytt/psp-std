package psp
package std

import java.{ lang => jl }

/** Rather than struggle with ambiguities with Predef.augmentString, we'll
 *  bury it and reimplement what we want.
 */
final class PspStringOps(val xs: String) extends AnyVal with Ops.SeqLikeOps[Char] {
  def elemAt(index: Index): Char = xs charAt index.value

  private def augment = Predef augmentString xs
  private def chars   = xs.toCharArray

  private def unwrapArg(arg: Any): AnyRef = arg match {
    case x: ScalaNumber => x.underlying
    case x: AnyRef      => x
  }
  private def slice(from: Int, until: Int): String = {
    val start = from max 0
    if (until <= start || length <= start) ""
    else xs.substring(start, length min until)
  }

  private def isEmpty = onull == ""
  private def onull   = if (xs eq null) "" else xs

  def replaceAllLiterally(literal: String, replacement: String): String = {
    import java.util.regex.Pattern
    import java.util.regex.Matcher

    val arg1 = Pattern quote literal
    val arg2 = Matcher quoteReplacement replacement
    xs.replaceAll(arg1, arg2)
  }

  def head: Char         = xs charAt 0
  def tail: String       = xs substring 1
  def init: String       = xs.substring(0, xs.length - 1)
  def last: Char         = xs charAt xs.length - 1
  def capitalize: String = if (isEmpty) "" else head.toUpper.toString  + tail

  def stripAnsi: String                     = ansi.Ansi strip xs
  def stripMargin(marginChar: Char): String = augment stripMargin marginChar
  def stripMargin: String                   = stripMargin('|')
  def stripPrefix(prefix: String): String   = if (xs startsWith prefix) this drop prefix.length else xs
  def stripSuffix(suffix: String): String   = if (xs endsWith suffix) this dropRight suffix.length else xs

  def drop(n: Int): String                  = if (n <= 0) xs else if (xs.length <= n) "" else xs.substring(n)
  def dropRight(n: Int): String             = if (n <= 0) xs else if (xs.length <= n) "" else xs.substring(0, xs.length - n)
  def dropWhile(p: Char => Boolean): String = xs drop (augment count p)
  def take(n: Int): String                  = if (n <= 0) "" else if (n >= length) xs else xs.substring(0, n)
  def takeRight(n: Int): String             = if (n <= 0) "" else if (n >= length) xs else xs.substring(xs.length - n, n)
  def takeWhile(p: Char => Boolean): String = xs take (augment count p)

  def length                      = xs.length
  def bytes: Array[Byte]          = xs.getBytes
  def words: Vector[String]       = (xs.trim split "\\s+").toVector
  def wordSet: Set[String]        = words.toSet
  def lines: Vector[String]       = (xs split EOL).toVector
  def * (n: Int): String          = Range.inclusive(1, n) map (_ => xs) mkString ""
  def format(args : Any*): String = java.lang.String.format(xs, args map unwrapArg: _*)

  def splitChar(ch: Char): Vector[String] = (xs split s"[$ch]").toVector
  def dottedSegments: Vector[String]      = splitChar('.')
  def slashSegments: Vector[String]       = splitChar('/')

  def index(elem: Char): Index                    = Index(xs indexOf elem)
  def lastIndex(elem: Char): Index                = Index(xs lastIndexOf elem)
  def indexAtWhich(p: Char => Boolean): Index     = chars indexAtWhich p
  def lastIndexAtWhich(p: Char => Boolean): Index = chars lastIndexAtWhich p
  def hasElem(elem: Char): Boolean                = chars.m contains elem
  def containsChar(ch: Char): Boolean             = (xs indexOf ch) >= 0

  def mapLines(f: String => String): String = lines map f mkString EOL
  def map(f: Char => Char): String          = augment map f
  def flatMap(f: Char => String): String    = new StringBuilder doto (sb => augment foreach (x => sb append f(x))) result

  def apply(index: Index): Char        = xs charAt index.value
  def apply(range: IndexRange): String = (indexRange intersect range) |> (r => slice(r.startInt, r.endInt))

  def toInt: Int       = if (xs startsWith "0x") jl.Integer.parseInt(xs drop 2, 16) else jl.Integer parseInt xs
  def toLong: Long     = if (xs startsWith "0x") jl.Long.parseLong(xs drop 2, 16) else jl.Long parseLong xs
  def toDouble: Double = jl.Double parseDouble xs
  def toFloat: Float   = jl.Float parseFloat xs

  // def toBytes: PspStringAsBytes = new PspStringAsBytes(xs)
  // def toChars: PspStringAsChars = new PspStringAsChars(xs)
  // def toLines: PspStringAsLines = new PspStringAsLines(xs)
  def toFile: jFile             = new jFile(xs)

  def from_s[A](implicit reads: Read[A]): A = reads read xs
  def to[A](implicit reads: Read[A]): A = reads read xs

  def trimTrailing: String = {
    if (length == 0 || !last.isWhitespace) xs
    else {
      var idx = length - 1
      while (idx >= 0 && xs.charAt(idx).isWhitespace)
        idx -= 1

      xs.substring(0, idx + 1)
    }
  }
  def sanitize: String = onull map (x => if (x.isControl) '?' else x)

  def truncateAndLeftJustifyTo(maxlen: Int): String =
    ("%-" + maxlen + "s").format(normalizeSpace truncateTo maxlen)

  def truncateTo(maxlen: Int): String =
    if (length <= maxlen) xs else xs.take(maxlen - 3) + "..."

  def normalizeSpace: String = (
    xs.trim
      .replaceAll("\\n+", "\n")
      .replaceAll("([{(])\\n", "$1 ")
      .replaceAll("\\n", "; ")
      .replaceAll("\\s+", " ")
  )

  override def toString = xs
}
