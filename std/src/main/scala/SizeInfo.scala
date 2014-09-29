package psp
package std

import api._

object SizeInfo {
  val Zero     = Size(0)
  val One      = Size(1)
  val Empty    = Precise(Zero)
  val Unknown  = Bounded(Zero, Infinite)
  val NonEmpty = Bounded(One, Infinite)

  def apply(n: Int): SizeInfo = Precise(Size(n))
  def apply(x: Any): SizeInfo = x match {
    case x: api.HasSizeInfo                 => x.sizeInfo
    case xs: scala.collection.IndexedSeq[_] => Size(xs.size)
    case xs: Traversable[_]                 => if (xs.isEmpty) Zero else NonEmpty
    case _                                  => Unknown
  }

  object GenBounded {
    def unapply(x: SizeInfo): Option[(api.Size, api.Atomic)] = x match {
      case Bounded(lo, hi) => Some(lo -> hi)
      case Precise(n)      => Some(n -> Precise(n))
      case _               => None
    }
  }
  // Return (lo, hi) as sizes unless arg is or contains Infinite.
  object Finite {
    def unapply(x: SizeInfo): Option[(api.Size, api.Size)] = x match {
      case Bounded(lo, Precise(hi)) => Some(lo -> hi)
      case Precise(n)               => Some(n -> n)
      case _                        => None
    }
  }
}
