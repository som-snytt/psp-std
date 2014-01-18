package psp
package view

import psp.core._
import psp.core.linear.PspList
import SizeInfo._

// TODO: TakeWhile, DropWhile, Distinct, Reverse, Zip
//
// -Xexperimental SAM
//   abstract class FlatMapF1[-T, +R] { def apply(x: T): Foreach[R] }
// final class StaticSizeFlatMap[-A, +B](val size: Size, f: A => Foreach[B]) extends (A => Foreach[B]) with HasStaticSize {
//   def apply(x: A): Foreach[B] = f(x)
// }

object Identity {
  def unapply[A](xs: Foreach[A]): Option[Foreach[A]] = xs match {
    case ReprId(xs, f)  => Some(f(xs))
    case IndexedId(xs)  => Some(xs)
    case LinearId(xs)   => Some(xs)
    case Labeled(xs, _) => unapply(xs)
    case _              => None
  }
}

final case class IndexedId[A](xs: Indexed[A]) extends ViewOp[Indexed, A](ss"<${xs.shortClass}>")
final case class LinearId[A](xs: Foreach[A]) extends ViewOp[Foreach, A](ss"<${xs.shortClass}>")
final case class ReprId[Coll, A](xs: Coll, f: Coll => Foreach[A]) extends ViewOp[Foreach, A]("ReprId")

final case class Labeled    [CC[X], A   ](xs: ViewOp[CC, A], label: String)            extends ViewOp[CC, A](label)
final case class Joined     [CC[X], A   ](xs: ViewOp[CC, A], ys: Foreach[A])           extends ViewOp[CC, A](ss"$xs ++ $ys")
final case class Filtered   [CC[X], A   ](xs: ViewOp[CC, A], p: A => Boolean)          extends ViewOp[CC, A](ss"$xs filter $p")
final case class Sliced     [CC[X], A   ](xs: ViewOp[CC, A], start: Index, end: Index) extends ViewOp[CC, A](ss"$xs slice ($start, $end)") { require(start >= 0 && end >= 0, this) }
final case class Dropped    [CC[X], A   ](xs: ViewOp[CC, A], n: Int)                   extends ViewOp[CC, A](ss"$xs drop $n") { require(n >= 0, this) }
final case class Taken      [CC[X], A   ](xs: ViewOp[CC, A], n: Int)                   extends ViewOp[CC, A](ss"$xs take $n") { require(n >= 0, this) }
final case class DroppedR   [CC[X], A   ](xs: ViewOp[CC, A], n: Int)                   extends ViewOp[CC, A](ss"$xs dropR $n") { require(n >= 0, this) }
final case class TakenR     [CC[X], A   ](xs: ViewOp[CC, A], n: Int)                   extends ViewOp[CC, A](ss"$xs takeR $n") { require(n >= 0, this) }
final case class Reversed   [CC[X], A   ](xs: ViewOp[CC, A])                           extends ViewOp[CC, A](ss"($xs).reverse")
final case class Mapped     [CC[X], A, B](xs: ViewOp[CC, A], f: A => B)                extends ViewOp[CC, B](ss"$xs map $f")
final case class FlatMapped [CC[X], A, B](xs: ViewOp[CC, A], f: A => Foreach[B])       extends ViewOp[CC, B](ss"$xs flatMap $f")
final case class Collected  [CC[X], A, B](xs: ViewOp[CC, A], pf: A =?> B)              extends ViewOp[CC, B](ss"$xs collect $pf")

object ViewOp {
  def repr[Coll](xs: Coll)(implicit itl: IsTraversableLike[Coll]): ReprId[Coll, itl.A] = ReprId(xs, xs => Foreach.traversable[itl.A]((itl conversion xs).seq.toTraversable))
  def linear[CC[X] <: Foreach[X], A](xs: CC[A]): LinearId[A]                           = LinearId(xs)
  def indexed[CC[X] <: Indexed[X], A](xs: CC[A]): IndexedId[A]                         = IndexedId(xs)
}

sealed abstract class ViewOp[CCIn[X], A](description: String) extends Viewable[A] with Foreach[A] {
  type CC[X] = ViewOp[CCIn, X]

  def sizeInfo: SizeInfo = this match {
    case Identity(xs)           => xs.sizeInfo
    case Taken(xs, n)           => xs.sizeInfo min precise(n)
    case TakenR(xs, n)          => xs.sizeInfo min precise(n)
    case Dropped(xs, n)         => xs.sizeInfo - precise(n)
    case DroppedR(xs, n)        => xs.sizeInfo - precise(n)
    case Sliced(xs, start, end) => xs.sizeInfo.slice(start, end)
    case Collected(xs, _)       => xs.sizeInfo.atMost
    case Filtered(xs, _)        => xs.sizeInfo.atMost
    case Joined(xs, ys)         => xs.sizeInfo + ys.sizeInfo
    case Mapped(xs, _)          => xs.sizeInfo
    case FlatMapped(xs, _)      => if (xs.sizeInfo.isZero) precise(0) else SizeInfo.Unknown
    case Reversed(xs)           => xs.sizeInfo
    case xs                     => xs.sizeInfo
  }

  private def foreachSlice[A](xs: Foreach[A], f: A => Unit, range: Interval): Unit = xs match {
    case Mapped(xs, g)  => foreachSlice(xs, g andThen f, range)
    case Identity(xs)   => foreachSlice(xs, f, range)
    case xs: Indexed[A] => var i = range.start ; while (i < range.end) { f(xs elemAt i) ; i += 1 }
    case _              =>
      var i = 0
      xs foreach { x =>
        if (range contains i) f(x)
        i += 1
        if (i >= range.end) return
      }
  }
  private def foreachTakeRight[A](xs: Foreach[A], f: A => Unit, n: Int): Unit = xs match {
    case Identity(xs)  => foreachTakeRight(xs, f, n)
    case Mapped(xs, g) => foreachTakeRight(xs, g andThen f, n)
    case _             => (CircularBuffer[A](Size(n)) ++= xs) foreach f
  }
  private def foreachDropRight[A](xs: Foreach[A], f: A => Unit, n: Int): Unit = xs match {
    case Identity(xs)  => foreachDropRight(xs, f, n)
    case Mapped(xs, g) => foreachDropRight(xs, g andThen f, n)
    case _             => xs.foldl(CircularBuffer[A](Size(n)))((buf, x) => if (buf.isFull) try buf finally f(buf push x) else buf += x)
  }

  final def foreach(f: A => Unit): Unit = {
    // println("%-20s  %s".format(sizeInfo, this))
    if (sizeInfo.isZero) return

    this match {
      case Identity(xs)            => xs foreach f
      case Mapped(xs, g)           => xs foreach (g andThen f)
      case FlattenSlice(xs, range) => foreachSlice(xs, f, range)
      case Reversed(xs)            => ???
      case FlatMapped(xs, g)       => xs foreach (x => g(x) foreach f)
      case Filtered(xs, p)         => xs foreach (x => if (p(x)) f(x))
      case Collected(xs, pf)       => xs foreach (x => if (pf isDefinedAt x) pf(x))
      case Joined(xs, ys)          => xs foreach f ; ys foreach f
      case DroppedR(xs, n)         => foreachDropRight(xs, f, n)
      case TakenR(xs, n)           => foreachTakeRight(xs, f, n)
      case Dropped(xs, n)          => foreachSlice(xs, f, Interval(n, Int.MaxValue))
      case Taken(xs, n)            => foreachSlice(xs, f, Interval(0, n))
      case Sliced(xs, start, end)  => foreachSlice(xs, f, Interval(start, end))
      case xs                      => xs foreach f
    }
  }

  final def map[B](f: A => B)                 = Mapped(this, f)
  final def flatMap[B](f: A => Foreach[B])    = FlatMapped(this, f)
  final def collect[B](pf: A =?> B)           = Collected(this, pf)
  final def withFilter(p: A => Boolean): Self = Filtered(this, p)
  final def filter(p: A => Boolean): Self     = Filtered(this, p)
  final def ++(that: Foreach[A])              = Joined(this, that)
  final def drop(n: Int): Self                = Dropped(this, n max 0)
  final def take(n: Int): Self                = Taken(this, n max 0)
  final def dropRight(n: Int): Self           = DroppedR(this, n max 0)
  final def takeRight(n: Int): Self           = TakenR(this, n max 0)
  final def slice(start: Int, end: Int): Self = Sliced(this, start max 0, end max 0)
  final def labeled(label: String): Self      = Labeled(this, label)
  final def reverse: Self                     = Reversed(this)

  final def psp: this.type = this
  final def force[That](implicit cb: CBF[Nothing, A, That]): That = {
    val buf = cb()
    this foreach (buf += _)
    buf.result
  }

  override def toString = description
}

object FlattenSlice {
  def unapply[CC[X], A](xs: Foreach[A]): Option[(Foreach[A], Interval)] = xs match {
    case Identity(xs)           => unapply(xs)
    case Mapped(xs, f)          => unapply(xs) map { case (xs, range) => (xs map f, range) }
    case DroppedR(xs, n)        => unapply(xs) map { case (xs, range) => (xs, range dropRight n) }
    case TakenR(xs, n)          => unapply(xs) map { case (xs, range) => (xs, range takeRight n) }
    case Dropped(xs, n)         => unapply(xs) map { case (xs, range) => (xs, range drop n) }
    case Taken(xs, n)           => unapply(xs) map { case (xs, range) => (xs, range take n) }
    case Sliced(xs, start, end) => unapply(xs) map { case (xs, range) => (xs, range.slice(start, end)) }
    case _                      => xs.sizeInfo.precisely map (size => xs -> Interval(0, size))
  }
}
