package psp
package std

import java.{ lang => jl }
import scala.collection.immutable.{ WrappedString, StringLike, StringOps }

/** Rather than struggle with ambiguities with Predef.augmentString, we'll
 *  bury it and reimplement what we want.
 */
final class PspStringOps(private val xs: String) extends AnyVal with SeqLikeExtensionOps[Char] {
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

  def stripAnsi: String                     = ansi.Ansi strip xs
  def stripMargin(marginChar: Char): String = augment stripMargin marginChar
  def stripMargin: String                   = stripMargin('|')
  def stripPrefix(prefix: String): String   = if (xs startsWith prefix) this drop prefix.length else xs
  def stripSuffix(suffix: String): String   = if (xs endsWith suffix) this dropRight suffix.length else xs
  def drop(n: Int): String                  = if (n <= 0) xs else if (xs.length <= n) "" else xs.substring(n)
  def dropRight(n: Int): String             = if (n <= 0) xs else if (xs.length <= n) "" else xs.substring(0, xs.length - n)

  def length                      = xs.length
  def bytes: Array[Byte]          = xs.getBytes
  def words: Vector[String]       = (xs.trim split "\\s+").toVector
  def lines: Vector[String]       = (xs split EOL).toVector
  def * (n: Int): String          = Range.inclusive(1, n) map (_ => xs) mkString ""
  def format(args : Any*): String = java.lang.String.format(xs, args map unwrapArg: _*)

  def index(elem: Char): Index                    = Index(xs indexOf elem)
  def lastIndex(elem: Char): Index                = Index(xs lastIndexOf elem)
  def indexAtWhich(p: Char => Boolean): Index     = Index(chars indexWhere p)
  def lastIndexAtWhich(p: Char => Boolean): Index = Index(chars lastIndexWhere p)
  def hasElem(elem: Char): Boolean                = chars contains elem

  def apply(index: Index): Char        = xs charAt index.value
  def apply(range: IndexRange): String = (indexRange intersect range) |> (r => slice(r.startInt, r.endInt))

  def toInt: Int       = if (xs startsWith "0x") jl.Integer.parseInt(xs drop 2, 16) else jl.Integer parseInt xs
  def toLong: Long     = if (xs startsWith "0x") jl.Long.parseLong(xs drop 2, 16) else jl.Long parseLong xs
  def toDouble: Double = jl.Double parseDouble xs
  def toFloat: Float   = jl.Float parseFloat xs

  def to[A](implicit reads: Read[A]): A = reads read xs

  override def toString = xs
}
