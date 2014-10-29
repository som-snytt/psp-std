package psp
package std

import api._, StdEq._

object Size {
  val Empty    = newSize(0)
  val NonEmpty = Bounded(newSize(1), Infinite)
  val Unknown  = Bounded(Empty, Infinite)

  def empty: Size   = Empty
  def unknown: Size = Unknown

  def min(s1: Precise, s2: Precise): Precise = if (s1 <= s2) s1 else s2
  def max(s1: Precise, s2: Precise): Precise = if (s1 >= s2) s1 else s2

  def hash(s: Size): Int = s match {
    case Precise(n) => n.##
    case x          => x.##
  }
  def equiv(s1: Size, s2: Size): Boolean = (s1, s2) match {
    case (Infinite, Infinite)               => true
    case (Precise(s1), Precise(s2))         => s1 === s2
    case (Bounded(l1, h1), Bounded(l2, h2)) => (l1 === l2) && (h1 === h2)
    case _                                  => false
  }

  def apply(n: Long): LongSize = Precise(n)
  def apply(n: Int): IntSize   = Precise(n)
  def apply(x: Any): Size = x match {
    case xs: HasSize                                => xs.size
    case xs: jCollection[_]                         => newSize(xs.size)
    case xs: scIndexedSeq[_]                        => newSize(xs.size)
    case xs: scTraversable[_] if xs.hasDefiniteSize => if (xs.isEmpty) Empty else NonEmpty
    case _                                          => Unknown
  }

  object GenBounded {
    def unapply(x: Size): Option[(Precise, Atomic)] = x match {
      case Bounded(lo, hi) => Some(lo -> hi)
      case x: Precise      => Some(x -> x)
      case _               => None
    }
  }
  // Return (lo, hi) as sizes unless arg is or contains Infinite.
  object Finite {
    def unapply(x: Size): Option[(Precise, Precise)] = x match {
      case Bounded(lo, hi: Precise) => Some(lo -> hi)
      case x: Precise               => Some(x -> x)
      case _                            => None
    }
  }

  // no, infinity doesn't really equal infinity, but it can for our
  // purposes as long as <inf> - <inf> is ill-defined.
  def partialCompare(lhs: Size, rhs: Size): PCmp = (lhs, rhs) match {
    case (Infinite, Infinite)             => PCmp.EQ
    case (Precise(_), Infinite)           => PCmp.LT
    case (Infinite, Precise(_))           => PCmp.GT
    case (Precise(x), Precise(y))         => if (x < y) PCmp.LT else if (y < x) PCmp.GT else PCmp.EQ
    case (Infinite, Bounded(_, Infinite)) => PCmp.NA
    case (Infinite, _)                    => PCmp.GT
    case (Bounded(_, Infinite), Infinite) => PCmp.NA
    case (_, Infinite)                    => PCmp.LT
    case (GenBounded(lo1, h1), GenBounded(lo2, h2)) =>
      if (h1 p_< lo2) PCmp.LT else if (h2 p_< lo1) PCmp.GT else PCmp.NA
  }

  def bounded(lo: Size, hi: Size): Size = (lo, hi) match {
    case _ if lo === hi                     => lo
    case (lo: Precise, hi: Atomic)          => Bounded(lo, hi)
    case (l1: Precise, Bounded(l2, h2))     => Bounded(l1 min l2, h2)
    case (Infinite, Bounded(_, Infinite))   => Infinite
    case (Infinite, _)                      => Empty
    case (Bounded(l1, h1), Infinite)        => Bounded(l1, Infinite)
    case (Bounded(l1, h1), h2: Precise)     => Bounded(l1, h2)
    case (Bounded(l1, h1), Bounded(l2, h2)) => Bounded(l1, h2)
  }

  final class Ops(val lhs: Size) extends AnyVal {
    def isNonZero    = !loBound.isZero
    def isZero       = lhs match {
      case Precise(0L) => true
      case _           => false
    }
    def isFinite     = lhs.hiBound !== Infinite
    def atMost: Size = bounded(Empty, lhs)
    def hiBound: Atomic = lhs match {
      case Bounded(_, hi) => hi
      case x: Atomic      => x
    }
    def loBound: Atomic = lhs match {
      case Bounded(lo, _) => lo
      case x: Atomic      => x
    }

    def mapAtomic(f: Precise => Precise, g: Atomic => Atomic): Size = lhs match {
      case x: Atomic       => g(x)
      case Bounded(lo, hi) => Bounded(f(lo), g(hi))
    }

    /** For instance taking the union of two sets. The new size is
     *  at least the size of the larger operand, but at most the sum
     *  of the two sizes.
     */
    def union(rhs: Size): Size     = bounded(lhs max rhs, lhs + rhs)
    def intersect(rhs: Size): Size = bounded(0.size, lhs min rhs)
    def diff(rhs: Size): Size      = bounded(lhs - rhs, lhs)

    def * (m: Long): Size = lhs match {
      case Precise(n)                        => newSize(n * m)
      case Bounded(Precise(lo), Precise(hi)) => bounded(newSize(lo * m), newSize(hi * m))
      case Bounded(Precise(lo), Infinite)    => if (m == 0L) unknown else bounded(newSize(lo * m), Infinite)
      case Infinite                          => if (m == 0L) unknown else Infinite
    }
    def * (rhs: Size): Size = lhs match {
      case Precise(n)                        => this * n
      case Bounded(Precise(lo), Precise(hi)) => bounded(rhs * lo, rhs * hi)
      case Bounded(Precise(lo), Infinite)    => if (rhs.isZero) unknown else bounded(rhs * lo, Infinite)
      case Infinite                          => if (rhs.isZero) unknown else Infinite
    }

    def + (rhs: Size): Size = (lhs, rhs) match {
      case (Infinite, _) | (_, Infinite)            => Infinite
      case (Precise(l), Precise(r))                 => newSize(l + r)
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 + l2, h1 + h2)
    }
    def - (rhs: Size): Size = (lhs, rhs) match {
      case (Infinite, Finite(_, _))         => Infinite
      case (Finite(_, _), Infinite)         => Empty
      case (Finite(l1, h1), Finite(l2, h2)) => bounded(l1 - h2, h1 - l2)
      case (Bounded(l1, h1), rhs: Precise)  => bounded(l1 - rhs, h1 - rhs)
      case _                                => unknown
    }

    def min(rhs: Size): Size = (lhs, rhs) match {
      case (Infinite, _)                            => rhs
      case (_, Infinite)                            => lhs
      case (Precise(x), Precise(y))                 => if (x <= y) lhs else rhs
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 min l2, h1 min h2)
    }
    def max(rhs: Size): Size = (lhs, rhs) match {
      case (Infinite, _)                            => Infinite
      case (_, Infinite)                            => Infinite
      case (Precise(x), Precise(y))                 => if (x >= y) lhs else rhs
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 max l2, h1 max h2)
    }
  }
}
