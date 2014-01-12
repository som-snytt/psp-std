package psp
package core

import PartialOrder._
import SizeInfo._
import ThreeValue._

/** The SizeInfo hierarchy is:
 *                   SizeInfo
 *                  /        \
 *               Atomic      Bounded
 *              /      \
 *          Infinite  Precise
 *
 *  Precise implies the exact size is known. Infinite means it's infinite.
 *  Bounded is a size lower bound and a (possibly infinite) atomic upper bound.
 *  SizeInfo forms a partial order, with some liberties taken at present.
 */
sealed trait SizeInfo extends PartiallyOrdered[SizeInfo] {
  // no, infinity doesn't really equal infinity, but it can for our
  // purposes as long as <inf> - <inf> is ill-defined.
  def partialCompare(that: SizeInfo): PartialOrder.Cmp = (this, that) match {
    case (Infinite, Infinite)                     => EQ
    case (Precise(_), Infinite)                   => LT
    case (Infinite, Precise(_))                   => GT
    case (Precise(x), Precise(y))                 => if (x < y) LT else if (y < x) GT else EQ
    case (Infinite, Bounded(_, Infinite))         => GE
    case (Infinite, _)                            => GT
    case (Bounded(_, Infinite), Infinite)         => LE
    case (_, Infinite)                            => LT
    case (GenBounded(l1, h1), GenBounded(l2, h2)) =>
      def lo1 = Precise(l1)
      def lo2 = Precise(l2)

      ( if (h1 < lo2 isTrue) LT
        else if (h1 <= lo2 isTrue) LE
        else if (h2 < lo1 isTrue) GT
        else if (h2 <= lo1 isTrue) GE
        else NA
      )
  }
}
sealed trait Atomic extends SizeInfo

trait HasSizeInfo extends Any { def sizeInfo: SizeInfo }

object SizeInfo {
  lazy val Empty = precise(0)
  lazy val Unknown = bounded(Zero, Infinite)

  def apply(x: Any): SizeInfo = x match {
    case x: HasSizeInfo => x.sizeInfo
    case _              => Unknown
  }
  def bounded(lo: Size, hi: SizeInfo): SizeInfo = hi match {
    case hi: Atomic     => bounded(lo, hi)
    case Bounded(_, hi) => bounded(lo, hi)
  }
  def bounded(lo: SizeInfo, hi: SizeInfo): SizeInfo = lo match {
    case Precise(lo)    => bounded(lo, hi)
    case Bounded(lo, _) => bounded(lo, hi)
    case Infinite       => Infinite
  }
  def bounded(lo: Size, hi: Atomic): SizeInfo = hi match {
    case Precise(n) if n < lo  => Empty
    case Precise(n) if n == lo => hi
    case _                     => Bounded(lo, hi)
  }

  final case object Infinite extends Atomic {
    override def toString = "<inf>"
  }
  final case class Precise(size: Size) extends Atomic {
    def +(amount: Size): Precise = Precise(size + amount)
    def -(amount: Size): Precise = Precise(size - amount)
    override def toString = s"$size"
  }
  final case class Bounded(lo: Size, hi: Atomic) extends SizeInfo {
    require((Precise(lo) < hi).isTrue, s"!($lo < $hi)")
    override def toString = s"[$lo, $hi)"
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

  implicit final class SizeInfoOps(val lhs: SizeInfo) extends AnyVal {
    def isZero = lhs == precise(0)
    def atMost: SizeInfo  = bounded(Zero, lhs)
    def atLeast: SizeInfo = bounded(lhs, Infinite)

    def + (rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, _) | (_, Infinite)            => Infinite
      case (Precise(l), Precise(r))                 => Precise(l + r)
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 + l2, h1 + h2)
    }
    def - (rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, Finite(_, _))         => Infinite
      case (Finite(_, _), Infinite)         => Empty
      case (Finite(l1, h1), Finite(l2, h2)) => bounded(l1 - h2, Precise(h1 - l2))
      case (Bounded(l1, h1), Precise(n))    => bounded(l1 - n, h1 - Precise(n))
      case _                                => Unknown
    }
    def min(rhs: SizeInfo): SizeInfo = lhs partialCompare rhs match {
      case LT | LE | EQ => lhs
      case GT | GE      => rhs
      case _            => onBounds(rhs)((l1, h1, l2, h2) => bounded(l1 min l2, h1 min h2))
    }
    def max(rhs: SizeInfo): SizeInfo = lhs partialCompare rhs match {
      case LT | LE | EQ => rhs
      case GT | GE      => lhs
      case _            => onBounds(rhs)((l1, h1, l2, h2) => bounded(l1 max l2, h1 max h2))
    }

    private def onBounds[T](rhs: SizeInfo)(f: (Size, Atomic, Size, Atomic) => T): T = {
      val GenBounded(l1, h1) = lhs
      val GenBounded(l2, h2) = rhs

      f(l1, h1, l2, h2)
    }
  }
}
