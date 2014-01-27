package psp
package core

import SizeInfo._

// TODO: TakeWhile, DropWhile, Distinct, Reverse, Zip
//
// -Xexperimental SAM
//   abstract class FlatMapF1[-T, +R] { def apply(x: T): Foreach[R] }
// final class StaticSizeFlatMap[-A, +B](val size: Size, f: A => Foreach[B]) extends (A => Foreach[B]) with HasStaticSize {
//   def apply(x: A): Foreach[B] = f(x)
// }

trait CountCalls {
  val counter: Counter

  def calls                  = counter.count
  def recordCall[T](x: T): T = counter record x
}

trait AtomicView[Coll, A] extends View[Coll, A] with CountCalls {
  val counter: Counter = new Counter()
  val tc: Foreachable[Coll]

  def toForeach: Foreach[A]
  def repr: Coll
  def atomicView: AtomicView[Coll, _] = this
  def sizeInfo = repr match {
    case xs: HasSizeInfo => xs.sizeInfo
    case _               => SizeInfo.Unknown
  }
  final def m: this.type = this
}

sealed abstract class CompositeView[Coll, +A](name: String, arg: String) extends View[Coll, A] {
  def atomicView: AtomicView[Coll, _] = this match {
    case xs: AtomicView[Coll, _] => xs
    case _                       => underlying.atomicView
  }

  def sizeInfo: SizeInfo = this match {
    case Sized(_, Size(n))  => precise(n)
    case LabeledView(xs, _) => xs.sizeInfo
    case Taken(xs, n)       => xs.sizeInfo min Precise(n)
    case TakenR(xs, n)      => xs.sizeInfo min Precise(n)
    case Dropped(xs, n)     => xs.sizeInfo - Precise(n)
    case DroppedR(xs, n)    => xs.sizeInfo - Precise(n)
    case Sliced(xs, range)  => xs.sizeInfo slice range
    case Collected(xs, _)   => xs.sizeInfo.atMost
    case Filtered(xs, _)    => xs.sizeInfo.atMost
    case Joined(xs, ys)     => xs.sizeInfo + ys.sizeInfo
    case Mapped(xs, _)      => xs.sizeInfo
    case FlatMapped(xs, _)  => if (xs.sizeInfo.isZero) precise(0) else SizeInfo.Unknown
    case Reversed(xs)       => xs.sizeInfo
    case xs                 => xs.sizeInfo
  }

  def underlying: View[Coll, _]
  def calls = underlying.calls
  def operationString = f"$name%-7s $arg%-7s"
  def description = if (arg == "") name else s"$name $arg"
  def completeString: String = this match {
    case LabeledView(_, label) => label
    case xs: AtomicView[_,_]   => xs.shortClass
    case _                     => pp"${underlying.completeString}   $operationString"
  }
  final def toForeach: Foreach[A] = new ViewForeach(this)
  override def toString = completeString
}

final class IndexedView[Coll, CC[X], A](val repr: Coll)(implicit val tc: IndexableType[Coll, CC, A]) extends AtomicView[Coll, A] {
  def size = tc length repr
  override def sizeInfo = Precise(size)
  def elemAt(index: Index): A = recordCall(tc.elemAt(repr)(index))

  def interval = Interval(0, size.value)
  def foreach(f: A => Unit): Unit = interval foreach (i => f(elemAt(i)))
  def foreachSlice(range: Interval)(f: A => Unit): Unit = range foreach (i => f(elemAt(i)))
  def toForeach: Indexed[A] = Indexed(size, elemAt)

  def completeString = "<xs>"
  override def toString = "<xs>"
}

object IndexedView {
  def apply[Coll](repr: Coll)(implicit tc: Indexable[Coll]): IndexedView[Coll, tc.CC, tc.A] = new IndexedView[Coll, tc.CC, tc.A](repr)(tc)
}
object LinearView {
  def apply[Coll](repr: Coll)(implicit ix: Foreachable[Coll]): LinearView[Coll, ix.CC, ix.A] = new LinearView[Coll, ix.CC, ix.A](repr)(ix)
}
object SetView {
  def apply[Coll](repr: Coll)(implicit tc: ForeachableSet[Coll]): SetView[Coll, tc.CC, tc.A] = new SetView[Coll, tc.CC, tc.A](repr)(tc)
}

final class LinearView[Coll, CC[X], A](val repr: Coll)(implicit val tc: ForeachableType[Coll, CC, A]) extends AtomicView[Coll, A] {
  def completeString = "<xs>"
  def toForeach: Foreach[A] = Foreach(f => tc.foreach(repr)(x => f(recordCall(x))))

  def foreachSlice(range: Interval)(f: A => Unit): Unit = {
    var i = 0
    tc.foreach(repr) { x =>
      if (range contains i) f(x)
      i += 1
      if (i >= range.end) return
    }
  }
}

final class SetView[Coll, CC[X], A](val repr: Coll)(implicit val tc: ForeachableSetType[Coll, CC, A]) extends AtomicView[Coll, A] {
  def completeString = pp"linear view of ${repr.shortClass}"
  def toForeach: Foreach[A] = Foreach(f => tc.foreach(repr)(x => f(recordCall(x))))
}

final case class LabeledView[Coll, +A   ](underlying: View[Coll, A], label: String)      extends CompositeView[Coll, A](label, "")
final case class Sized      [Coll, +A   ](underlying: View[Coll, A], size: Size)         extends CompositeView[Coll, A]("sized", pp"$size")
final case class Joined     [C1, C2, +A ](underlying: View[C1, A], ys: View[C2, A])      extends CompositeView[C1, A]("++", pp"$ys")
final case class Filtered   [Coll,  A   ](underlying: View[Coll, A], p: A => Boolean)    extends CompositeView[Coll, A]("filter", pp"$p")
final case class Sliced     [Coll, +A   ](underlying: View[Coll, A], range: Interval)    extends CompositeView[Coll, A]("slice", pp"$range")
final case class Dropped    [Coll, +A   ](underlying: View[Coll, A], n: Size)            extends CompositeView[Coll, A]("drop", pp"$n")
final case class Taken      [Coll, +A   ](underlying: View[Coll, A], n: Size)            extends CompositeView[Coll, A]("take", pp"$n")
final case class DroppedR   [Coll, +A   ](underlying: View[Coll, A], n: Size)            extends CompositeView[Coll, A]("dropR", pp"$n")
final case class TakenR     [Coll, +A   ](underlying: View[Coll, A], n: Size)            extends CompositeView[Coll, A]("takeR", pp"$n")
final case class Reversed   [Coll, +A   ](underlying: View[Coll, A])                     extends CompositeView[Coll, A]("reverse", "")
final case class Mapped     [Coll,  A, B](underlying: View[Coll, A], f: A => B)          extends CompositeView[Coll, B]("map", pp"$f")
final case class FlatMapped [Coll,  A, B](underlying: View[Coll, A], f: A => Foreach[B]) extends CompositeView[Coll, B]("flatMap", pp"$f")
final case class Collected  [Coll,  A, B](underlying: View[Coll, A], pf: A =?> B)        extends CompositeView[Coll, B]("collect", pp"$pf")

class ViewForeach[Coll, A](view: View[Coll, A]) extends Foreach[A] {
  override def toString = pp"$view foreach _"

  private def foreachSlice[Coll, A](xs: View[Coll, A], f: A => Unit, range: Interval): Unit = {
    var i = 0
    def runThrough(xs: Foreach[A]): Boolean = {
      xs foreach { x =>
        if (range contains i) f(x)
        i += 1
        if (i >= range.end) return true
      }
      i >= range.end
    }

    xs match {
      case xs: IndexedView[_, _, _] => xs.foreachSlice(xs.interval slice range)(f)
      case xs: LinearView[_, _, _]  => runThrough(xs.toForeach)
      case Mapped(xs, g)            => foreachSlice(xs, g andThen f, range)
      case xs: Indexed[A]           => var i = range.start ; while (i < range.end) { f(xs elemAt i) ; i += 1 }
      case Joined(ys1, ys2)         =>
      ys1.sizeInfo.precisely match {
        case Some(n) if n < range.start => ys2 slice (range - n) foreach f
        case Some(n) if n > range.end   => ys1 slice range foreach f
        case _                          => runThrough(ys1) || runThrough(ys2)
      }
      case _              => runThrough(xs)
    }
  }

  private def foreachTakeRight[A](xs: Foreach[A], f: A => Unit, n: Size): Unit = (CircularBuffer[A](n) ++= xs) foreach f

  private def foreachDropRight[A](xs: Foreach[A], f: A => Unit, n: Size): Unit =
    xs.foldl(CircularBuffer[A](n))((buf, x) => if (buf.isFull) try buf finally f(buf push x) else buf += x)

  def sizeInfo = view.sizeInfo

  final def foreach(f: A => Unit): Unit = {
    if (sizeInfo.isZero) return

    def loop[Coll, B](xs: View[Coll, B])(f: B => Unit): Unit = xs match {
      case xs: AtomicView[Coll, B]                => xs.toForeach foreach f
      case LabeledView(xs, _)                     => loop(xs)(f)
      case Sized(xs, size)                        => loop(xs)(f)
      case Mapped(xs, g)                          => loop(xs)(g andThen f)
      case FlatMapped(xs, g)                      => loop(xs)(x => g(x) foreach f)
      case Filtered(xs, p: Function1[B, Boolean]) => loop(xs)(x => if (p(x)) f(x))
      case Collected(xs, pf)                      => loop(xs)(x => if (pf isDefinedAt x) pf(x))
      case FlattenIndexedSlice(xs, range)         => foreachSlice(xs, f, range)
      case Reversed(xs)                           => ???
      case Joined(xs, ys)                         => loop(xs)(f) ; loop(ys)(f)
      case DroppedR(xs, Size(n))                  => foreachDropRight(xs, f, Size(n))
      case TakenR(xs, Size(n))                    => foreachTakeRight(xs, f, Size(n))
      case Dropped(xs, Size(n))                   => foreachSlice(xs, f, Interval.Full drop n)
      case Taken(xs, Size(n))                     => foreachSlice(xs, f, Interval.Full take n)
      case Sliced(xs, range)                      => foreachSlice(xs, f, range)
    }
    loop(view)(f)
  }
}

trait ElementalView[+A] extends Any {
  type MapTo[+X] <: ElementalView[X]

  def map[B](f: A => B): MapTo[B]
  def flatMap[B](f: A => Foreach[B]): MapTo[B]
  def collect[B](pf: A =?> B): MapTo[B]
  def withFilter(p: A => Boolean): MapTo[A]
  def filter(p: A => Boolean): MapTo[A]
  def drop(n: Int): MapTo[A]
  def take(n: Int): MapTo[A]
  def dropRight(n: Int): MapTo[A]
  def takeRight(n: Int): MapTo[A]
  def slice(start: Int, end: Int): MapTo[A]
  def slice(range: Interval): MapTo[A]
  def labeled(label: String): MapTo[A]
  def sized(size: Size): MapTo[A]
  def reverse: MapTo[A]

  def calls: Int
  def sizeInfo: SizeInfo
  def toForeach: Foreach[A]
}

sealed trait View[Coll, +A] extends ElementalView[A] {
  type MapTo[+B] = View[Coll, B]

  def completeString: String
  def atomicView: AtomicView[Coll, _]

  final def map[B](f: A => B): MapTo[B]               = Mapped(this, f)
  final def flatMap[B](f: A => Foreach[B]): MapTo[B]  = FlatMapped(this, f)
  final def collect[B](pf: A =?> B): MapTo[B]         = Collected(this, pf)
  final def ++[A1 >: A](that: Foreach[A1]): MapTo[A1] = Joined(this, that.m)

  final def withFilter(p: A => Boolean): MapTo[A] = Filtered(this, p)
  final def filter(p: A => Boolean): MapTo[A]     = Filtered(this, p)
  final def drop(n: Int): MapTo[A]                = Dropped(this, Size(n))
  final def take(n: Int): MapTo[A]                = Taken(this, Size(n))
  final def dropRight(n: Int): MapTo[A]           = DroppedR(this, Size(n))
  final def takeRight(n: Int): MapTo[A]           = TakenR(this, Size(n))
  final def slice(start: Int, end: Int): MapTo[A] = Sliced(this, Interval(start, end))
  final def slice(range: Interval): MapTo[A]      = Sliced(this, range)
  final def labeled(label: String): MapTo[A]      = LabeledView(this, label)
  final def sized(size: Size): MapTo[A]           = Sized(this, size)
  final def reverse: MapTo[A]                     = Reversed(this)

  final def native(implicit pcb: PspCanBuild[A, Coll]): Coll      = force[Coll]
  final def force[That](implicit pcb: PspCanBuild[A, That]): That = pcb build toForeach
}

trait LowPriorityView {
  implicit def implicitViewForce[Coll, A, That](xs: View[Coll, A])(implicit pcb: PspCanBuild[A, That]): That = xs.force
  implicit def implicitViewNative[Coll, A](xs: View[Coll, A])(implicit pcb: PspCanBuild[A, Coll]): Coll      = xs.native
}

object View extends LowPriorityView {
  implicit def implicitViewForeachOps[Coll, A](xs: View[Coll, A]): ForeachOperations[A]                      = new ForeachOperations(xs.toForeach)
  implicit def implicitViewForeachConversions[Coll, A](xs: View[Coll, A]): ForeachConversions[A]             = new ForeachConversions(xs.toForeach)
  implicit def implicitViewForeach[Coll, A](xs: View[Coll, A]): Foreach[A]                                   = xs.toForeach
}

object FlattenIndexedSlice {
  def unapply[Coll, A](xs: View[Coll, A]): Option[(View[Coll, A], Interval)] = xs match {
    case xs: IndexedView[_, _, _] => Some(xs -> xs.interval)
    case LabeledView(xs, _)       => unapply(xs)
    case Sized(xs, Size(n))       => Some(xs -> Interval(0, n))
    case Mapped(xs, f)            => unapply(xs) map { case (xs, range) => (xs map f, range) }
    case DroppedR(xs, Size(n))    => unapply(xs) map { case (xs, range) => (xs, range dropRight n) }
    case TakenR(xs, Size(n))      => unapply(xs) map { case (xs, range) => (xs, range takeRight n) }
    case Dropped(xs, Size(n))     => unapply(xs) map { case (xs, range) => (xs, range drop n) }
    case Taken(xs, Size(n))       => unapply(xs) map { case (xs, range) => (xs, range take n) }
    case Sliced(xs, indices)      => unapply(xs) map { case (xs, range) => (xs, range slice indices) }
    case _                        => xs.sizeInfo.precisely map (size => xs -> Interval(0, size))
  }
}
