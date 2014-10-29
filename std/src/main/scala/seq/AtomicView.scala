package psp
package std

import Size._, api._
import lowlevel.CircularBuffer
import psp.std.StdShow._

sealed abstract class AtomicView[A, Repr] extends View.Atomic[A] with BaseView[A, Repr] with ops.InvariantViewOps[A] {
  type This <: AtomicView[A, Repr]
  def counting: CounterView[A, Repr, This] = new CounterView(this.castTo[This], new RecorderCounter)
  def foreachSlice(range: IndexRange)(f: A => Unit): IndexRange
}

final class CounterView[A, Repr, V <: AtomicView[A, Repr]](val underlying: V, val counter: RecorderCounter) extends AtomicView[A, Repr] {
  type This = CounterView[A, Repr, V]

  def calls   = counter.count
  def viewOps = underlying.viewOps
  def size    = underlying.size

  @inline def foreach(f: A => Unit): Unit                               = underlying foreach (x => f(counter record x))
  @inline def foreachSlice(range: IndexRange)(f: A => Unit): IndexRange = underlying.foreachSlice(range)(x => f(counter record x))

  def debug_s = underlying.debug_s
}

final case class SplitView[+A, Repr](left: BaseView[A, Repr], right: BaseView[A, Repr]) extends View.Split[A] {
  type Single[+X] = BaseView[X, Repr]

  def mapLeft[A1 >: A](f: Single[A1] => Single[A1]): SplitView[A1, Repr]  = SplitView(f(left), right)
  def mapRight[A1 >: A](f: Single[A1] => Single[A1]): SplitView[A1, Repr] = SplitView(left, f(right))
  def join: Single[A]                                                     = Joined(left, right)
  def intersperse: Single[A]                                              = Interspersed(left, right)
  def zipped: Single[(A, A)]                                              = Zipped(left, right)
  def force[That](implicit z: Builds[A, That]): That                      = z build join
}

object FlattenSlice {
  def unapply[A, Repr](xs: BaseView[A, Repr]): Option[(BaseView[A, Repr], IndexRange)] = xs match {
    case xs: IndexedView[_, _] => Some(xs -> xs.indices)
    case LabeledView(xs, _)    => unapply(xs)
    case Mapped(xs, f)         => unapply(xs) map { case (xs, range) => (xs map f, range) }
    case DroppedR(xs, n)       => unapply(xs) map { case (xs, range) => (xs, range dropRight n) }
    case TakenR(xs, n)         => unapply(xs) map { case (xs, range) => (xs, range takeRight n) }
    case Dropped(xs, n)        => unapply(xs) map { case (xs, range) => (xs, range drop n) }
    case Taken(xs, n)          => unapply(xs) map { case (xs, range) => (xs, range take n) }
    case HasSize(n: Precise)   => Some(xs -> n.indices)
    case _                     => None
  }
}

final class LinearView[A, Repr](underlying: Each[A]) extends AtomicView[A, Repr] {
  type This   = LinearView[A, Repr]
  def viewOps = Direct("<list>")
  def size    = underlying.size

  @inline def foreach(f: A => Unit): Unit                       = linearlySlice(underlying, IndexRange.full, f)
  def foreachSlice(range: IndexRange)(f: A => Unit): IndexRange = linearlySlice(underlying, range, f)
}

final class ExSetView[A, Repr](underlying: ExtensionalSet[A]) extends AtomicView[A, Repr] with SetView[A] {
  type This                                                     = ExSetView[A, Repr]
  def viewOps                                                   = Direct("<set>")
  def size                                                      = underlying.size
  @inline def foreach(f: A => Unit): Unit                       = linearlySlice(underlying, IndexRange.full, f)
  def foreachSlice(range: IndexRange)(f: A => Unit): IndexRange = linearlySlice(underlying, range, f)
}

final class IndexedView[A, Repr](underlying: Direct[A]) extends AtomicView[A, Repr] with Direct[A] with ops.HasPreciseSizeMethods {
  type This = IndexedView[A, Repr]

  def viewOps                                                   = Direct("<vector>")
  def size: Precise                                             = underlying.size
  def elemAt(i: Index): A                                       = underlying(i)
  def foreach(f: A => Unit): Unit                               = directlySlice(underlying, size.indices, f)
  def foreachSlice(range: IndexRange)(f: A => Unit): IndexRange = directlySlice(underlying, range, f)
}

final case class LabeledView[A, Repr](prev: BaseView[A, Repr], val viewOps: Direct[String]) extends BaseView[A, Repr] {
  type This = LabeledView[A, Repr]
  def foreach(f: A => Unit): Unit = prev foreach f
  def description                 = viewOps.last
  def size                        = prev.size
}

sealed trait BaseView[+A, Repr] extends AnyRef with View[A] with ops.ApiViewOps[A] {
  type This <: BaseView[A, Repr]

  def xs: this.type = this

  type MapTo[+X]   = BaseView[X, Repr]
  type SplitTo[+X] = SplitView[X, Repr]

  def |:(label: String): MapTo[A] = new LabeledView(this, viewOps.init :+ label)
  def :|(label: String): MapTo[A] = new LabeledView(this, viewOps.init :+ label)

  final def ++[A1 >: A](that: View[A1]): MapTo[A1]  = Joined(this, that)
  final def collect[B](pf: A ?=> B): MapTo[B]       = Collected(this, pf)
  final def drop(n: Precise): MapTo[A]              = Dropped(this, n)
  final def dropRight(n: Precise): MapTo[A]         = DroppedR(this, n)
  final def dropWhile(p: Predicate[A]): MapTo[A]    = DropWhile(this, p)
  final def flatMap[B](f: A => Each[B]): MapTo[B]   = FlatMapped(this, f)
  // final def intersperse(that: View[A]): MapTo[A] = Interspersed(this, that)
  final def map[B](f: A => B): MapTo[B]             = Mapped(this, f)
  final def take(n: Precise): MapTo[A]              = Taken(this, n)
  final def takeRight(n: Precise): MapTo[A]         = TakenR(this, n)
  final def takeWhile(p: Predicate[A]): MapTo[A]    = TakenWhile(this, p)
  final def withFilter(p: Predicate[A]): MapTo[A]   = Filtered(this, p)
  final def zip[B](that: View[B]): MapTo[(A, B)]    = Zipped(this, that)

  final def dropIndex(index: Index): MapTo[A]      = s"dropIndex $index" |: (index.toSize |> (s => SplitView(take(s), drop(s.increment)).join))
  final def splitAt(index: Index): SplitTo[A]      = index.toSize |> (s => SplitView(take(s), drop(s)))
  final def span(p: Predicate[A]): SplitTo[A]      = SplitView(takeWhile(p), dropWhile(p))
  final def partition(p: Predicate[A]): SplitTo[A] = SplitView(withFilter(p), withFilter(!p))

  final def force[That](implicit z: Builds[A, That]): That = z build this
  final def build(implicit z: Builds[A, Repr]): Repr       = force[Repr]

  def linearlySlice[A](xs: Each[A], range: IndexRange, f: A => Unit): IndexRange = {
    var dropping  = range.startInt
    var remaining = range.intSize
    def nextRange = indexRange(dropping, dropping + remaining)
    xs foreach { x =>
      if (dropping > 0)
        dropping = dropping - 1
      else if (remaining > 0)
        try f(x) finally remaining -= 1
      else
        return nextRange
    }
    nextRange
  }
  def directlySlice[A](xs: Direct[A], range: IndexRange, f: A => Unit): IndexRange = {
    xs.indices slice range foreach (i => f(xs(i)))
    range << xs.intSize
  }
}

sealed abstract class CompositeView[A, B, Repr](val description: String, val sizeEffect: Unary[Size]) extends View.Composite[A, B] with BaseView[B, Repr] {
  def prev: View[A]
  def size    = sizeEffect(prev.size)
  def viewOps = prev.viewOps :+ description

  final def foreach(f: B => Unit): Unit = {
    def loop[C](xs: View[C])(f: C => Unit): Unit = xs match {
      case FlattenSlice(xs, range)         => foreachSlice(xs, range, f)
      case LabeledView(xs, _)              => loop[C](xs)(f)
      case Mapped(xs, g)                   => loop(xs)(g andThen f)
      case FlatMapped(xs, g)               => loop(xs)(x => g(x) foreach f)
      case Filtered(xs, p: Predicate[C])   => loop(xs)(x => if (p(x)) f(x))
      case TakenWhile(xs, p: Predicate[C]) => foreachTakeWhile(xs, f, p)
      case DropWhile(xs, p: Predicate[C])  => foreachDropWhile(xs, f, p)
      case Collected(xs, pf)               => loop(xs)(x => if (pf isDefinedAt x) f(pf(x)))
      case Joined(xs, ys)                  => loop(xs)(f) ; loop(ys)(f)
      case DroppedR(xs, n: Precise)        => foreachDropRight(xs, f, n)
      case TakenR(xs, n: Precise)          => foreachTakeRight(xs, f, n)
      case Dropped(xs, Precise(n))         => foreachSlice(xs, Index(n) until MaxIndex, f)
      case Taken(xs, n: Precise)           => foreachSlice(xs, n.indices, f)
      case Zipped(_, _)                    => foreachSlice(xs, IndexRange.full, f)
      case xs: View[_]                     => xs foreach f
      case _                               => abort(pp"Unexpected view class ${xs.shortClass}")
    }

    if (!size.isZero)
      loop(this)(f)
  }

  private def foreachTakeWhile[A](xs: Each[A], f: A => Unit, p: Predicate[A]): Int = {
    var taken = 0
    xs foreach { x =>
      if (p(x)) f(x) sideEffect (taken += 1)
      else return taken
    }
    taken
  }
  private def foreachDropWhile[A](xs: Each[A], f: A => Unit, p: Predicate[A]): Int = {
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

  private def foreachTakeRight[A](xs: Each[A], f: A => Unit, n: Precise): Unit =
    (CircularBuffer[A](n) ++= xs) foreach f

  private def foreachDropRight[A](xs: Each[A], f: A => Unit, n: Precise): Unit =
    xs.foldl(CircularBuffer[A](n))((buf, x) => if (buf.isFull) try buf finally f(buf push x) else buf += x)

  private def foreachSlice[A](xs: View[A], range: IndexRange, f: A => Unit): Unit = xs match {
    case Zipped(xs, ys)                                             => xs.iterator zip ys.iterator drop range.startInt take range.size.intSize foreach f
    case xs: AtomicView[_, _]                                       => xs.foreachSlice(range)(f)
    case Mapped(prev, g)                                            => foreachSlice(prev, range, g andThen f)
    case xs: Direct[_]                                              => directlySlice(xs, range, f)
    case Joined(HasSize(PreciseInt(n)), ys) if n < range.startInt   => ys slice (range << n) foreach f
    case Joined(ys @ HasSize(PreciseInt(n)), _) if range.endInt < n => ys slice range foreach f
    case Joined(ys1, ys2)                                           => linearlySlice(ys1, range, f) |> (remainingRange => linearlySlice(ys2, remainingRange, f))
    case _                                                          => linearlySlice(xs, range, f)
  }

}

final case class Interspersed[A   , Repr](prev: BaseView[A, Repr], ys: View[A])     extends CompositeView[A, A, Repr](pp"intersperse $ys", _ + ys.size)
final case class Zipped      [A, B, Repr](prev: BaseView[A, Repr], ys: View[B])     extends CompositeView[A, (A, B), Repr](pp"zip $ys", _ min ys.size)
final case class Joined [A, B >: A, Repr](prev: BaseView[A, Repr], ys: View[B])     extends CompositeView[A, B, Repr](pp"++ $ys", _ + ys.size)
final case class Filtered    [A   , Repr](prev: BaseView[A, Repr], p: Predicate[A]) extends CompositeView[A, A, Repr](pp"filter $p",    _.atMost)
final case class Dropped     [A   , Repr](prev: BaseView[A, Repr], n: Precise)      extends CompositeView[A, A, Repr](pp"drop $n",      _ - n)
final case class DroppedR    [A   , Repr](prev: BaseView[A, Repr], n: Precise)      extends CompositeView[A, A, Repr](pp"dropR $n",     _ - n)
final case class Taken       [A   , Repr](prev: BaseView[A, Repr], n: Precise)      extends CompositeView[A, A, Repr](pp"take $n",      _ min n)
final case class TakenR      [A   , Repr](prev: BaseView[A, Repr], n: Precise)      extends CompositeView[A, A, Repr](pp"takeR $n",     _ min n)
final case class TakenWhile  [A   , Repr](prev: BaseView[A, Repr], p: Predicate[A]) extends CompositeView[A, A, Repr](pp"takeW $p",     _.atMost)
final case class DropWhile   [A   , Repr](prev: BaseView[A, Repr], p: Predicate[A]) extends CompositeView[A, A, Repr](pp"dropW $p",     _.atMost)
final case class Mapped      [A, B, Repr](prev: BaseView[A, Repr], f: A => B)       extends CompositeView[A, B, Repr](pp"map $f",       x => x)
final case class FlatMapped  [A, B, Repr](prev: BaseView[A, Repr], f: A => Each[B]) extends CompositeView[A, B, Repr](pp"flatMap $f",   x => if (x.isZero) x else Unknown)
final case class Collected   [A, B, Repr](prev: BaseView[A, Repr], pf: A ?=> B)     extends CompositeView[A, B, Repr](pp"collect $pf",  _.atMost)
