package psp
package std

import PartialOrder._

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
 *  Operations on sizes which are ill-defined will result in "Unknown", which
 *  encodes no available size information: Bounded(Zero, Infinite).
 */
sealed trait SizeInfo {
  def +(size: Size): SizeInfo
}

// This arrangement means that "x + size" results in the same type as x for any SizeInfo.
sealed trait Atomic                            extends SizeInfo { def +(n: Size): Atomic }
final case class Bounded(lo: Size, hi: Atomic) extends SizeInfo { def +(n: Size): Bounded       = Bounded(lo + n, hi + n) }
final case class Precise(size: Size)           extends Atomic   { def +(n: Size): Precise       = Precise(size + n)       }
final case object Infinite                     extends Atomic   { def +(n: Size): Infinite.type = Infinite                }

trait HasSizeInfo extends Any { def sizeInfo: SizeInfo }
trait HasPreciseSize extends Any with HasSizeInfo { def size: Size }
trait HasStaticSize[N <: Nat] extends HasPreciseSize

trait HasPreciseSizeImpl extends Any with HasPreciseSize {
  final override def sizeInfo = Precise(size)
}

object SizeInfo {
  val Zero     = Size(0)
  val One      = Size(1)
  val Empty    = Precise(Zero)
  val Unknown  = Bounded(Zero, Infinite)
  val NonEmpty = Bounded(One, Infinite)

  def apply(n: Int): SizeInfo = Precise(Size(n))

  implicit def ShowSizeInfo[A <: SizeInfo]: Show[A] = Show[A] {
    case Bounded(lo, Infinite) => show"[$lo, <inf>)"
    case Bounded(lo, hi)       => show"[$lo, $hi]"
    case Precise(size)         => s"$size"
    case Infinite              => "<inf>"
  }

  // no, infinity doesn't really equal infinity, but it can for our
  // purposes as long as <inf> - <inf> is ill-defined.
  implicit object SizeInfoPartialOrder extends PartialOrder[SizeInfo] {
    def partialCompare(lhs: SizeInfo, rhs: SizeInfo): PartialOrder.Cmp = (lhs, rhs) match {
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

  def apply(x: Any): SizeInfo = x match {
    case x: HasSizeInfo                     => x.sizeInfo
    case xs: scala.collection.IndexedSeq[_] => Size(xs.size)
    case xs: Traversable[_]                 => if (xs.isEmpty) Zero else NonEmpty
    case _                                  => Unknown
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

  implicit def sizeInfoOperations(lhs: SizeInfo): SizeInfoOperations = new SizeInfoOperations(lhs)
}

final class SizeInfoOperations(val lhs: SizeInfo) extends AnyVal {
  import PartialOrder._
  import psp.std.SizeInfo._

  def isZero    = lhs == Precise(Zero)
  def isFinite  = lhs.hiBound != Infinite
  def isPrecise = lhs match { case _: Precise => true ; case _ => false }

  def atMost: SizeInfo  = bounded(Zero, lhs)
  def atLeast: SizeInfo = bounded(lhs, Infinite)
  def hiBound: Atomic = lhs match {
    case Bounded(_, hi) => hi
    case x: Atomic      => x
  }

  def slice(range: IndexRange): SizeInfo = (lhs - range.start.toSize) min range.size

  def precisely: Option[Int] = lhs match { case Precise(Size(n)) => Some(n) ; case _ => None }
  def preciseOr(alt: => Int): Int = precisely getOrElse alt
  def preciseIntSize: Int = preciseOr(sys error s"precise size unavailable: $lhs")

  def * (rhs: Size): SizeInfo = lhs match {
    case Precise(n)               => Precise(n * rhs.value)
    case Bounded(lo, Precise(hi)) => Bounded(lo * rhs.value, Precise(hi * rhs.value))
    case Bounded(lo, _)           => if (rhs.isZero) Unknown else Bounded(lo * rhs.value, Infinite)
    case Infinite                 => if (rhs.isZero) Unknown else Infinite
  }

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

