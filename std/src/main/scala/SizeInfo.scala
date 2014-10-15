package psp
package std

import api._

object SizeInfo {
  val Zero     = Size(0)
  val One      = Size(1)
  val Empty    = Precise(Zero)
  val Unknown  = Bounded(Zero, Infinite)
  val NonEmpty = Bounded(One, Infinite)

  def unknown: SizeInfo                      = Unknown
  def precise(n: Int): Precise               = Precise(Size(n))
  def min(s1: Precise, s2: Precise): Precise = if (s1 <= s2) s1 else s2
  def max(s1: Precise, s2: Precise): Precise = if (s1 >= s2) s1 else s2

  // no, infinity doesn't really equal infinity, but it can for our
  // purposes as long as <inf> - <inf> is ill-defined.
  def partialCompare(lhs: SizeInfo, rhs: SizeInfo): PCmp = (lhs, rhs) match {
    case (Infinite, Infinite)                     => PCmp.EQ
    case (Precise(_), Infinite)                   => PCmp.LT
    case (Infinite, Precise(_))                   => PCmp.GT
    case (Precise(x), Precise(y))                 => if (x < y) PCmp.LT else if (y < x) PCmp.GT else PCmp.EQ
    case (Infinite, Bounded(_, Infinite))         => PCmp.NA
    case (Infinite, _)                            => PCmp.GT
    case (Bounded(_, Infinite), Infinite)         => PCmp.NA
    case (_, Infinite)                            => PCmp.LT
    case (GenBounded(lo1, h1), GenBounded(lo2, h2)) =>
      if (h1 p_< Precise(lo2)) PCmp.LT else if (h2 p_< Precise(lo1)) PCmp.GT else PCmp.NA
  }

  def apply(n: Int): SizeInfo = Precise(Size(n))
  def apply(x: Any): SizeInfo = x match {
    case x: HasSizeInfo                     => x.sizeInfo
    case xs: scala.collection.IndexedSeq[_] => Size(xs.size)
    case xs: scTraversable[_]               => if (xs.isEmpty) Zero else NonEmpty
    case _                                  => Unknown
  }

  object GenBounded {
    def unapply(x: SizeInfo): Option[(Size, Atomic)] = x match {
      case Bounded(lo, hi) => Some(lo -> hi)
      case Precise(n)      => Some(n -> Precise(n))
      case _               => None
    }
  }
  // Return (lo, hi) as sizes unless arg is or contains Infinite.
  object Finite {
    def unapply(x: SizeInfo): Option[(Size, Size)] = x match {
      case Bounded(lo, Precise(hi)) => Some(lo -> hi)
      case Precise(n)               => Some(n -> n)
      case _                        => None
    }
  }
}
