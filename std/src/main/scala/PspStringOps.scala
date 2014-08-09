package psp
package std

import scala.compat.Platform.EOL
import scala.math.ScalaNumber
import scala.collection.immutable.{ WrappedString, StringLike, StringOps }

/** Rather than struggle with ambiguities with Predef.augmentString, we'll
 *  bury it and reimplement what we want.
 */
final class PspStringOps(private val xs: String) extends AnyVal with SeqLikeExtensionOps[Char] {
  private def chars = xs.toCharArray

  private def unwrapArg(arg: Any): AnyRef = arg match {
    case x: ScalaNumber => x.underlying
    case x              => x.asInstanceOf[AnyRef]
  }
  private def slice(from: Int, until: Int): String = {
    val start = from max 0
    if (until <= start || length <= start) ""
    else xs.substring(start, length min until)
  }

  def length                      = xs.length
  def lines: Iterator[String]     = (xs split EOL).iterator
  def * (n: Int): String          = 1 to n map (_ => xs) mkString ""
  def format(args : Any*): String = java.lang.String.format(toString, args map unwrapArg: _*)

  def index(elem: Char): Index                    = Index(xs indexOf elem)
  def lastIndex(elem: Char): Index                = Index(xs lastIndexOf elem)
  def indexAtWhich(p: Char => Boolean): Index     = Index(chars indexWhere p)
  def lastIndexAtWhich(p: Char => Boolean): Index = Index(chars lastIndexWhere p)
  def hasElem(elem: Char): Boolean                = chars contains elem

  def apply(index: Index): Char        = xs charAt index.value
  def apply(range: IndexRange): String = (indexRange intersect range) |> (r => slice(r.startInt, r.endInt))

  override def toString = xs
}
