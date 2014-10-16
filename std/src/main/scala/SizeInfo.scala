package psp
package std

import api._, StdEq._

object SizeInfo {
  val Empty    = newSize(0)
  val NonEmpty = Bounded(newSize(1), Infinite)
  val Unknown  = Bounded(Empty, Infinite)

  def empty: SizeInfo   = Empty
  def unknown: SizeInfo = Unknown

  def min(s1: PreciseSize, s2: PreciseSize): PreciseSize = if (s1 <= s2) s1 else s2
  def max(s1: PreciseSize, s2: PreciseSize): PreciseSize = if (s1 >= s2) s1 else s2

  def apply(n: Long): PreciseSize = newSize(n)
  def apply(x: Any): SizeInfo = x match {
    case xs: HasSizeInfo                            => xs.sizeInfo
    case xs: jCollection[_]                         => newSize(xs.size)
    case xs: scIndexedSeq[_]                        => newSize(xs.size)
    case xs: scTraversable[_] if xs.hasDefiniteSize => if (xs.isEmpty) Empty else NonEmpty
    case _                                          => Unknown
  }

  object GenBounded {
    def unapply(x: SizeInfo): Option[(PreciseSize, Atomic)] = x match {
      case Bounded(lo, hi) => Some(lo -> hi)
      case x: PreciseSize  => Some(x -> x)
      case _               => None
    }
  }
  // Return (lo, hi) as sizes unless arg is or contains Infinite.
  object Finite {
    def unapply(x: SizeInfo): Option[(PreciseSize, PreciseSize)] = x match {
      case Bounded(lo, hi: PreciseSize) => Some(lo -> hi)
      case x: PreciseSize               => Some(x -> x)
      case _                            => None
    }
  }

  // no, infinity doesn't really equal infinity, but it can for our
  // purposes as long as <inf> - <inf> is ill-defined.
  def partialCompare(lhs: SizeInfo, rhs: SizeInfo): PCmp = (lhs, rhs) match {
    case (Infinite, Infinite)             => PCmp.EQ
    case (PreciseSize(_), Infinite)       => PCmp.LT
    case (Infinite, PreciseSize(_))       => PCmp.GT
    case (PreciseSize(x), PreciseSize(y)) => if (x < y) PCmp.LT else if (y < x) PCmp.GT else PCmp.EQ
    case (Infinite, Bounded(_, Infinite)) => PCmp.NA
    case (Infinite, _)                    => PCmp.GT
    case (Bounded(_, Infinite), Infinite) => PCmp.NA
    case (_, Infinite)                    => PCmp.LT
    case (GenBounded(lo1, h1), GenBounded(lo2, h2)) =>
      if (h1 p_< lo2) PCmp.LT else if (h2 p_< lo1) PCmp.GT else PCmp.NA
  }

  def bounded(lo: SizeInfo, hi: SizeInfo): SizeInfo = (lo, hi) match {
    case _ if lo === hi                     => lo
    case (lo: PreciseSize, hi: Atomic)      => Bounded(lo, hi)
    case (l1: PreciseSize, Bounded(l2, h2)) => Bounded(l1 min l2, h2)
    case (Infinite, Bounded(_, Infinite))   => Infinite
    case (Infinite, _)                      => Empty
    case (Bounded(l1, h1), Infinite)        => Bounded(l1, Infinite)
    case (Bounded(l1, h1), h2: PreciseSize) => Bounded(l1, h2)
    case (Bounded(l1, h1), Bounded(l2, h2)) => Bounded(l1, h2)
  }

  final class Ops(val lhs: SizeInfo) extends AnyVal {
    def isZero           = lhs == Empty
    def isFinite         = lhs.hiBound != Infinite
    def atMost: SizeInfo = bounded(Empty, lhs)
    def hiBound: Atomic = lhs match {
      case Bounded(_, hi) => hi
      case x: Atomic      => x
    }

    def slice(range: IndexRange): SizeInfo = (lhs - range.precedingSize) min range.size

    def * (m: Long): SizeInfo = lhs match {
      case PreciseSize(n)                            => newSize(n * m)
      case Bounded(PreciseSize(lo), PreciseSize(hi)) => bounded(newSize(lo * m), newSize(hi * m))
      case Bounded(PreciseSize(lo), Infinite)        => if (m == 0L) unknown else bounded(newSize(lo * m), Infinite)
      case Infinite                                  => if (m == 0L) unknown else Infinite
    }
    def * (rhs: SizeInfo): SizeInfo = lhs match {
      case PreciseSize(n)                            => this * n
      case Bounded(PreciseSize(lo), PreciseSize(hi)) => bounded(rhs * lo, rhs * hi)
      case Bounded(PreciseSize(lo), Infinite)        => if (rhs.isZero) unknown else bounded(rhs * lo, Infinite)
      case Infinite                                  => if (rhs.isZero) unknown else Infinite
    }

    def + (rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, _) | (_, Infinite)            => Infinite
      case (PreciseSize(l), PreciseSize(r))         => newSize(l + r)
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 + l2, h1 + h2)
    }
    def - (rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, Finite(_, _))            => Infinite
      case (Finite(_, _), Infinite)            => Empty
      case (Finite(l1, h1), Finite(l2, h2))    => bounded(l1 - h2, h1 - l2)
      case (Bounded(l1, h1), rhs: PreciseSize) => bounded(l1 - rhs, h1 - rhs)
      case _                                   => unknown
    }

    def min(rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, _)                            => rhs
      case (_, Infinite)                            => lhs
      case (PreciseSize(x), PreciseSize(y))         => if (x <= y) lhs else rhs
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 min l2, h1 min h2)
    }
    def max(rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, _)                            => Infinite
      case (_, Infinite)                            => Infinite
      case (PreciseSize(x), PreciseSize(y))         => if (x >= y) lhs else rhs
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 max l2, h1 max h2)
    }
  }
}
