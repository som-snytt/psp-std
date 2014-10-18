package psp
package std

import SizeInfo._, api._
import lowlevel.CircularBuffer
import psp.std.StdShow._

final case class SplitView[+A, Repr](left: BaseView[A, Repr], right: BaseView[A, Repr]) extends View.Split[A] {
  type Single[+X] = BaseView[X, Repr]

  def mapLeft[A1 >: A](f: Unary[Single[A1]]): SplitView[A1, Repr]  = SplitView(f(left), right)
  def mapRight[A1 >: A](f: Unary[Single[A1]]): SplitView[A1, Repr] = SplitView(left, f(right))
  def join: Single[A]                                              = Joined(left, right)
  def intersperse: Single[A]                                       = Interspersed(left, right)
  def zipped: Single[(A, A)]                                       = Zipped(left, right)
  def force[That](implicit z: Builds[A, That]): That               = z build join
}

object FlattenIndexedSlice {
  def foreachTakeWhile[A](xs: Foreach[A], f: A => Unit, p: Predicate[A]): Int = {
    var taken = 0
    xs foreach { x =>
      if (p(x)) f(x) sideEffect (taken += 1)
      else return taken
    }
    taken
  }
  def foreachDropWhile[A](xs: Foreach[A], f: A => Unit, p: Predicate[A]): Int = {
    var dropping = true
    var dropped  = 0
    xs foreach { x =>
      if (dropping && p(x)) dropped += 1
      else {
        if (dropping) dropping = false
        f(x)
      }
    }
    dropped
  }

  def unapply[A, Repr](xs: BaseView[A, Repr]): Option[(BaseView[A, Repr], IndexRange)] = xs match {
    case xs: IndexedView[_, _]        => Some(xs -> xs.indices)
    case LabeledView(xs, _)           => unapply(xs)
    case Sized(xs, n)                 => Some(xs -> n.indices)
    case Mapped(xs, f)                => unapply(xs) map { case (xs, range) => (xs map f, range) }
    case DroppedR(xs, n: PreciseSize) => unapply(xs) map { case (xs, range) => (xs, range dropRight n) }
    case TakenR(xs, n: PreciseSize)   => unapply(xs) map { case (xs, range) => (xs, range takeRight n) }
    case Dropped(xs, n: PreciseSize)  => unapply(xs) map { case (xs, range) => (xs, range drop n) }
    case Taken(xs, n: PreciseSize)    => unapply(xs) map { case (xs, range) => (xs, range take n) }
    case Sliced(xs, indices)          => unapply(xs) map { case (xs, range) => (xs, range slice indices) }
    case _                            => xs.sizeInfo match { case n: PreciseSize => Some(xs -> n.indices) ; case _ => None }
  }
}

final class LinearView[A0, Repr](repr: Repr, val tc: Foreachable[Repr] { type A = A0 }) extends AtomicView[A0, Repr] {
  def description = ""
  def sizeInfo    = SizeInfo(repr)
  def viewRange   = IndexRange.full

  @inline def foreach(f: A => Unit): Unit = foreachSlice(IndexRange.full)(f)
  def foreachSlice(range: IndexRange)(f: A => Unit): Unit = {
    var i = Index.zero
    (tc foreach repr) { x =>
      recordCall(x)
      if (range contains i) f(x)
      i = i.next
      if (i >= range.end) return
    }
  }
}

final class IndexedView[A0, Repr](repr: Repr, val tc: DirectAccess[Repr] { type A = A0 }, val viewRange: IndexRange) extends AtomicView[A0, Repr] with Direct[A0] with HasPreciseSize with ops.HasPreciseSizeMethods {
  def size: PreciseSize           = SizeInfo.min(tc length repr, viewRange.size)
  def elemAt(i: Index): A         = recordCall(tc.elemAt(repr)(i))
  def contains(x: A): Boolean     = this exists (_ == x)
  def foreach(f: A => Unit): Unit = foreachSlice(size.indices)(f)
  def foreachSlice(range: IndexRange)(f: A => Unit): Unit = {
    val combined = viewRange slice range
    assert(size containsRange combined, s"($repr/size=$size).foreachSlice($range)($f)")
    combined foreach (i => f(elemAt(i)))
  }
  def description                                         = ""
}

final case class LabeledView[+A, Repr](prev: BaseView[A, Repr], label: String) extends BaseView[A, Repr] {
  def foreach(f: A => Unit): Unit = prev foreach f

  def description = label
  def isAtomic    = prev.isAtomic
  def sizeInfo    = prev.sizeInfo
  def calls       = prev.calls
  def viewChain   = newSeq(this) ++ (prev.viewChain drop 1)
  def viewRange   = prev.viewRange
}

sealed trait BaseView[+A, Repr] extends Any with View[A] with RearSliceable[BaseView[A, Repr]] {
  type MapTo[+X]   = BaseView[X, Repr]
  type SplitTo[+X] = SplitView[X, Repr]

  def isAtomic: Boolean
  def viewRange: IndexRange

  final def ++[A1 >: A](that: View[A1]): MapTo[A1]   = Joined(this, that)
  final def collect[B](pf: A ?=> B): MapTo[B]        = Collected(this, pf)
  final def count(p: Predicate[A]): PreciseSize      = takeWhile(p) |> (v => v.sizeInfo match { case x: PreciseSize => x ; case _ => v.force[pVector[A]].size })
  final def drop(n: PreciseSize): MapTo[A]           = Dropped(this, n)
  final def dropRight(n: PreciseSize): MapTo[A]      = DroppedR(this, n)
  final def dropWhile(p: Predicate[A]): MapTo[A]     = DropWhile(this, p)
  final def filter(p: Predicate[A]): MapTo[A]        = Filtered(this, p)
  final def filterNot(p: Predicate[A]): MapTo[A]     = Filtered(this, (x: A) => !p(x))
  final def flatMap[B](f: A => Foreach[B]): MapTo[B] = FlatMapped(this, f)
  final def map[B](f: A => B): MapTo[B]              = Mapped(this, f)
  final def sized(size: PreciseSize): MapTo[A]       = Sized(this, size)
  final def slice(range: IndexRange): MapTo[A]       = Sliced(this, range)
  final def take(n: PreciseSize): MapTo[A]           = Taken(this, n)
  final def takeRight(n: PreciseSize): MapTo[A]      = TakenR(this, n)
  final def takeWhile(p: Predicate[A]): MapTo[A]     = TakenWhile(this, p)
  final def withFilter(p: Predicate[A]): MapTo[A]    = Filtered(this, p)
  final def zip[B](that: View[B]): MapTo[(A, B)]     = Zipped(this, that)

  final def dropIndex(index: Index): MapTo[A]      = index.toSize |> (s => SplitView(take(s), drop(s.increment)).join)
  final def splitAt(index: Index): SplitTo[A]      = index.toSize |> (s => SplitView(take(s), drop(s)))
  final def span(p: Predicate[A]): SplitTo[A]      = SplitView(takeWhile(p), dropWhile(p))
  final def partition(p: Predicate[A]): SplitTo[A] = SplitView(filter(p), filterNot(p))

  final def force[That](implicit z: Builds[A, That]): That = z build this
  final def build(implicit z: Builds[A, Repr]): Repr       = force[Repr]

  override def toString = viewChain.pvec.reverse map (_.description) mkString " "
}

sealed abstract class AtomicView[A0, Repr] extends View.Atomic[A0] with BaseView[A0, Repr] with CountCalls {
  type A = tc.A
  val tc: Walkable[Repr]
  val counter = new RecorderCounter()

  def viewRange: IndexRange
  def foreachSlice(range: IndexRange)(f: A => Unit): Unit
  def isAtomic  = true
  def viewChain: Foreach[View[_]] = newSeq(this)
}

sealed abstract class CompositeView[A, B, Repr](val description: String, val sizeEffect: Unary[SizeInfo]) extends View.Composite[A, B] with BaseView[B, Repr] {
  def prev: View[A]

  def isAtomic                    = false
  def viewChain: Foreach[View[_]] = newSeq(this) ++ prev.viewChain
  def sizeInfo                    = sizeEffect(prev.sizeInfo)

  lazy val viewRange: IndexRange = {
    def loop[C](xs: View[C]): IndexRange = xs match {
      case Dropped(xs, n: PreciseSize)    => loop(xs) drop n
      case Taken(xs, n: PreciseSize)      => loop(xs) take n
      case Sliced(xs, range)              => loop(xs) slice range
      case DropWhile(xs, p: Predicate[C]) => loop(xs) drop (xs count p)
      case _                              => xs.viewRange
    }
    loop(this)
  }

  final def foreach(f: B => Unit): Unit = {
    def loop[C](xs: View[C])(f: C => Unit): Unit = xs match {

      case LabeledView(xs, _)              => loop[C](xs)(f)
      case Sized(xs, size)                 => loop[C](xs)(f)
      case Mapped(xs, g)                   => loop(xs)(g andThen f)
      case FlatMapped(xs, g)               => loop(xs)(x => g(x) foreach f)
      case Filtered(xs, p: Predicate[C])   => loop(xs)(x => if (p(x)) f(x))
      case TakenWhile(xs, p: Predicate[C]) => FlattenIndexedSlice.foreachTakeWhile(xs, f, p)
      case DropWhile(xs, p: Predicate[C])  => FlattenIndexedSlice.foreachDropWhile(xs, f, p)
      case Collected(xs, pf)               => loop(xs)(x => if (pf isDefinedAt x) f(pf(x)))
      case FlattenIndexedSlice(xs, range)  => foreachSlice(xs, f, range)
      case Joined(xs, ys)                  => loop(xs)(f) ; loop(ys)(f)
      case DroppedR(xs, n: PreciseSize)    => foreachDropRight(xs, f, n)
      case TakenR(xs, n: PreciseSize)      => foreachTakeRight(xs, f, n)
      case Dropped(xs, PreciseSize(n))     => foreachSlice(xs, f, Index(n) until MaxIndex)
      case Taken(xs, n: PreciseSize)       => foreachSlice(xs, f, n.indices)
      case Sliced(xs, range)               => foreachSlice(xs, f, range)
      case Zipped(_, _)                    => foreachSlice(xs, f, viewRange)
      case Interspersed(xs, ys)            => foreachSlice(xs, f, viewRange)
      case xs: View[_]                     => xs foreach f
      case xs                              => abort(pp"Unexpected view class ${xs.shortClass}")
    }

    if (!sizeInfo.isZero)
      loop(this)(f)
  }

  private def runThrough[A](f: A => Unit, range: IndexRange, xs: Foreach[A], others: Foreach[A]*): Unit = {
    var foreaches: List[Foreach[A]] = xs :: others.toList
    var i = Index.zero
    def isDone = range.end <= i
    while (foreaches.nonEmpty) {
      val current = foreaches.head
      foreaches = foreaches.tail
      current foreach { x =>
        if (range contains i) f(x)
        i = i.next
        if (isDone) return
      }
    }
  }
  private def foreachInterspersed[A](f: A => Unit, range: IndexRange, xs: Foreach[A], ys: Foreach[A]): Unit = {
    var i = Index.zero
    val it1 = xs.biIterator
    val it2 = ys.biIterator
    while (it1.hasNext && it2.hasNext && i < range.start) {
      it1.next
      it2.next
    }
    while (it1.hasNext && it2.hasNext && i < range.end) {
      f(it1.next)
      f(it2.next)
      i += 1
    }
    while ((it1.hasNext || it2.hasNext) && i < range.end) {
      f( if (it1.hasNext) it1.next else it2.next )
      i += 1
    }
  }

  private def foreachTakeRight[A](xs: Foreach[A], f: A => Unit, n: PreciseSize): Unit =
    (CircularBuffer[A](n) ++= xs) foreach f

  private def foreachDropRight[A](xs: Foreach[A], f: A => Unit, n: PreciseSize): Unit =
    xs.foldl(CircularBuffer[A](n))((buf, x) => if (buf.isFull) try buf finally f(buf push x) else buf += x)

  private def foreachSlice[A](xs: View[A], f: A => Unit, range: IndexRange): Unit = xs match {
    case Zipped(xs, ys)       => xs.biIterator zip ys.biIterator drop range.startInt take range.size.intSize foreach f
    case Interspersed(xs, ys) => foreachInterspersed(f, range, xs, ys)
    case xs: AtomicView[_, _] => xs.foreachSlice(range)(f.castTo[xs.A => Unit])
    // case xs: AtomicView[_, _] => runThrough(f, range, xs)
    case m: Mapped[a, _, _]   => foreachSlice[a](m.prev, m.f andThen f, range)
    case xs: Direct[A]        => range foreach (i => f(xs elemAt i))
    case Joined(ys1, ys2) =>
      ys1.sizeInfo match {
        case n: PreciseSize if n.intSize < range.startInt     => ys2 slice (range << n.intSize) foreach f
        case n: PreciseSize if n.lastIndex >= range.lastIndex => ys1 slice range foreach f
        case _                                                => runThrough(f, range, ys1, ys2)
      }

    case _              => runThrough(f, range, xs)
  }

  def calls = this match {
    case Joined(xs, ys) => xs.calls + ys.calls
    case _              => prev.calls
  }
}

final case class Zipped[A, B, Repr](prev: BaseView[A, Repr], ys: View[B]) extends CompositeView[A, (A, B), Repr](pp"zip $ys", _ min ys.sizeInfo)
final case class Joined[A, B >: A, Repr](prev: BaseView[A, Repr], ys: View[B])   extends CompositeView[A, B, Repr](pp"++ $ys", _ + ys.sizeInfo)

final case class Interspersed[A   , Repr](prev: BaseView[A, Repr], ys: View[A])        extends CompositeView[A, A, Repr](pp"intersperse $ys", _ + ys.sizeInfo)
final case class Sized       [A   , Repr](prev: BaseView[A, Repr], size: PreciseSize)  extends CompositeView[A, A, Repr](pp"sized $size",  _ => size)
final case class Filtered    [A   , Repr](prev: BaseView[A, Repr], p: Predicate[A])    extends CompositeView[A, A, Repr](pp"filter $p",    _.atMost)
final case class Sliced      [A   , Repr](prev: BaseView[A, Repr], range: IndexRange)  extends CompositeView[A, A, Repr](pp"slice $range", _ slice range)
final case class Dropped     [A   , Repr](prev: BaseView[A, Repr], n: PreciseSize)     extends CompositeView[A, A, Repr](pp"drop $n",      _ - n)
final case class DroppedR    [A   , Repr](prev: BaseView[A, Repr], n: PreciseSize)     extends CompositeView[A, A, Repr](pp"dropR $n",     _ - n)
final case class Taken       [A   , Repr](prev: BaseView[A, Repr], n: PreciseSize)     extends CompositeView[A, A, Repr](pp"take $n",      _ min n)
final case class TakenR      [A   , Repr](prev: BaseView[A, Repr], n: PreciseSize)     extends CompositeView[A, A, Repr](pp"takeR $n",     _ min n)
final case class TakenWhile  [A   , Repr](prev: BaseView[A, Repr], p: Predicate[A])    extends CompositeView[A, A, Repr](pp"takeW $p",     _.atMost)
final case class DropWhile   [A   , Repr](prev: BaseView[A, Repr], p: Predicate[A])    extends CompositeView[A, A, Repr](pp"dropW $p",     _.atMost)
final case class Mapped      [A, B, Repr](prev: BaseView[A, Repr], f: A => B)          extends CompositeView[A, B, Repr](pp"map $f",       x => x)
final case class FlatMapped  [A, B, Repr](prev: BaseView[A, Repr], f: A => Foreach[B]) extends CompositeView[A, B, Repr](pp"flatMap $f",   x => if (x.isZero) x else Unknown)
final case class Collected   [A, B, Repr](prev: BaseView[A, Repr], pf: A ?=> B)        extends CompositeView[A, B, Repr](pp"collect $pf",  _.atMost)
