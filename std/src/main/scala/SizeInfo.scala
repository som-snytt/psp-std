package psp
package std

import api._

object SizeInfo {
  val Zero     = Size(0)
  val One      = Size(1)
  val Empty    = Precise(0)
  val Unknown  = Bounded(Zero, Infinite)
  val NonEmpty = Bounded(One, Infinite)

  def unknown: SizeInfo        = Unknown
  def precise(n: Int): Precise = Precise(n)

  def min(s1: Precise, s2: Precise): Precise = if (s1 <= s2) s1 else s2
  def max(s1: Precise, s2: Precise): Precise = if (s1 >= s2) s1 else s2

  def apply(n: Int): Precise = Precise(n)
  def apply(x: Any): SizeInfo = x match {
    case xs: HasSizeInfo                            => xs.sizeInfo
    case xs: jCollection[_]                         => precise(xs.size)
    case xs: scIndexedSeq[_]                        => precise(xs.size)
    case xs: scTraversable[_] if xs.hasDefiniteSize => if (xs.isEmpty) Zero else NonEmpty
    case _                                          => Unknown
  }

  object GenBounded {
    def unapply(x: SizeInfo): Option[(Precise, Atomic)] = x match {
      case Bounded(lo, hi) => Some(lo.precisely -> hi)
      case x: Precise      => Some(x -> x)
      case _               => None
    }
  }
  // Return (lo, hi) as sizes unless arg is or contains Infinite.
  object Finite {
    def unapply(x: SizeInfo): Option[(Precise, Precise)] = x match {
      case Bounded(lo, hi: Precise) => Some(lo.precisely -> hi)
      case x: Precise               => Some(x -> x)
      case _                        => None
    }
  }

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
      if (h1 p_< lo2) PCmp.LT else if (h2 p_< lo1) PCmp.GT else PCmp.NA
  }

  def bounded(lo: Size, hi: SizeInfo): SizeInfo = hi match {
    case hi: Atomic     => bounded(lo, hi)
    case Bounded(_, hi) => bounded(lo, hi)
  }
  def bounded(lo: SizeInfo, hi: SizeInfo): SizeInfo = lo match {
    case lo: Precise    => bounded(lo, hi)
    case Bounded(lo, _) => bounded(lo, hi)
    case Infinite       => Infinite
  }
  def bounded(lo: Size, hi: Atomic): SizeInfo = hi match {
    case hi: Precise if hi < lo.precisely  => SizeInfo.Empty
    case hi: Precise if hi == lo.precisely => hi
    case _                                 => Bounded(lo, hi)
  }

  final class Ops(val lhs: SizeInfo) extends AnyVal {
    def isZero    = lhs == Precise(0)
    def isFinite  = lhs.hiBound != Infinite
    def isPrecise = lhs match { case _: Precise => true ; case _ => false }

    def atMost: SizeInfo  = bounded(Zero, lhs)
    def atLeast: SizeInfo = bounded(lhs, Infinite)
    def hiBound: Atomic = lhs match {
      case Bounded(_, hi) => hi
      case x: Atomic      => x
    }

    def slice(range: IndexRange): SizeInfo = (lhs - Size(range.head.indexValue)) min range.size

    def precisely: Option[Int] = lhs match { case Precise(n) => Some(n) ; case _ => None }
    def preciseOr(alt: => Int): Int = precisely | alt
    def preciseIntSize: Int = preciseOr(sys error s"precise size unavailable: $lhs")

    def * (rhs: Size): SizeInfo = lhs match {
      case Precise(n)               => precise(n * rhs.sizeValue)
      case Bounded(lo, Precise(hi)) => bounded(Size(lo.get * rhs.get), precise(hi * rhs.get))
      case Bounded(lo, _)           => if (rhs.isZero) Unknown else Bounded(lo * rhs.get, Infinite)
      case Infinite                 => if (rhs.isZero) Unknown else Infinite
    }

    def + (rhs: Size): SizeInfo = this + Precise(rhs.sizeValue)
    def + (rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, _) | (_, Infinite)            => Infinite
      case (Precise(l), Precise(r))                 => Precise(l + r)
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(Size(l1.get + l2.get), h1 + h2)
    }
    def - (rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, Finite(_, _))            => Infinite
      case (Finite(_, _), Infinite)            => Empty
      case (Finite(l1, h1), Finite(l2, h2))    => bounded(l1 - h2, h1 - l2)
      case (Bounded(l1, h1), rhs @ Precise(n)) => bounded(l1 - rhs, h1 - rhs)
      case _                                   => Unknown
    }

    def min(rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, _)                            => rhs
      case (_, Infinite)                            => lhs
      case (Precise(x), Precise(y))                 => if (x <= y) lhs else rhs
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 min l2, h1 min h2)
    }
    def max(rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, _)                            => Infinite
      case (_, Infinite)                            => Infinite
      case (Precise(x), Precise(y))                 => if (x >= y) lhs else rhs
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 max l2, h1 max h2)
    }
  }
}
