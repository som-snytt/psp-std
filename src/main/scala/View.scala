package psp
package core

import SizeInfo._

// TODO: Distinct, Reverse, Zip
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

trait Elemental[+A] extends Any with Foreach[A] {
  type MapTo[+X] <: Elemental[X]

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MapTo[B]
  def flatMap[B](f: A => Foreach[B]): MapTo[B]
  def collect[B](pf: A =?> B): MapTo[B]
  def withFilter(p: A => Boolean): MapTo[A]
  def filter(p: A => Boolean): MapTo[A]
  def filterNot(p: A => Boolean): MapTo[A]
  def drop(n: Int): MapTo[A]
  def take(n: Int): MapTo[A]
  def takeWhile(p: A => Boolean): MapTo[A]
  def dropWhile(p: A => Boolean): MapTo[A]
  def dropRight(n: Int): MapTo[A]
  def takeRight(n: Int): MapTo[A]
  def slice(start: Int, end: Int): MapTo[A]
  def slice(range: Interval): MapTo[A]
  def labeled(label: String): MapTo[A]
  def sized(size: Size): MapTo[A]
  def reverse: MapTo[A]

  def calls: Int
}

object AtomicView {
  def linear[Coll](repr: Coll)(implicit tc: Foreachable[Coll]): psp.core.LinearView[Coll, tc.CC, tc.A] = new ViewEnvironment[Coll, tc.CC, tc.A](repr) linearView tc
  def indexed[Coll](repr: Coll)(implicit tc: Indexable[Coll]): psp.core.IndexedView[Coll, tc.CC, tc.A] = new ViewEnvironment[Coll, tc.CC, tc.A](repr) indexedView tc
  def apply[Coll](repr: Coll)(implicit tc: Foreachable[Coll]): psp.core.AtomicView[Coll, tc.CC, tc.A]  = new ViewEnvironment[Coll, tc.CC, tc.A](repr) newView tc
}

class ViewEnvironment[Coll, CC[X], A](val repr: Coll) {
  def newView(tc: Foreachable[Coll]): AtomicView = tc match {
    case tc: Indexable[_]   => new IndexedView(tc.castTo[IndexableType[Coll, CC, A]])
    case tc: Foreachable[_] => new LinearView(tc.castTo[ForeachableType[Coll, CC, A]])
  }
  def linearView(tc: ForeachableType[Coll, CC, A]): LinearView = new LinearView(tc)
  def indexedView(tc: IndexableType[Coll, CC, A]): IndexedView = new IndexedView(tc)

  final class IndexedView(val tc: IndexableType[Coll, CC, A]) extends AtomicView with HasContains[A] with HasPreciseSize {
    final def m: this.type = this

    def size: Size                                        = tc length repr
    def elemAt(index: Index): A                           = recordCall(tc.elemAt(repr)(index))
    def contains(x: A): Boolean                           = this exists (_ == x)
    def foreach(f: A => Unit): Unit                       = foreachSlice(size.toInterval)(f)
    def foreachSlice(range: Interval)(f: A => Unit): Unit = range foreach (i => f(elemAt(i)))
  }

  final class LinearView(val tc: ForeachableType[Coll, CC, A]) extends AtomicView {
    final def m: this.type = this

    def sizeInfo = SizeInfo(repr)
    def foreach(f: A => Unit): Unit = foreachSlice(Interval.Full)(x => f(x))
    def foreachSlice(range: Interval)(f: A => Unit): Unit = {
      var i = 0
      (tc foreach repr) { x =>
        recordCall(x)
        if (range contains i) f(x)
        i += 1
        if (i >= range.end) return
      }
    }
  }

  sealed trait View[+A] extends Any with Elemental[A] {
    type MapTo[+B] = View[B]

    def completeString: String
    def atomicView: AtomicView

    final def map[B](f: A => B): MapTo[B]               = Mapped(this, f)
    final def flatMap[B](f: A => Foreach[B]): MapTo[B]  = FlatMapped(this, f)
    final def collect[B](pf: A =?> B): MapTo[B]         = Collected(this, pf)
    final def ++[A1 >: A](that: Foreach[A1]): MapTo[A1] = Joined(this, that.m)

    final def withFilter(p: A => Boolean): MapTo[A] = Filtered(this, p)
    final def filter(p: A => Boolean): MapTo[A]     = Filtered(this, p)
    final def filterNot(p: A => Boolean): MapTo[A]  = Filtered(this, (x: A) => !p(x))
    final def drop(n: Int): MapTo[A]                = Dropped(this, Size(n))
    final def take(n: Int): MapTo[A]                = Taken(this, Size(n))
    final def takeWhile(p: A => Boolean): MapTo[A]  = TakenWhile(this, p)
    final def dropWhile(p: A => Boolean): MapTo[A]  = DropWhile(this, p)
    final def dropRight(n: Int): MapTo[A]           = DroppedR(this, Size(n))
    final def takeRight(n: Int): MapTo[A]           = TakenR(this, Size(n))
    final def slice(start: Int, end: Int): MapTo[A] = Sliced(this, Interval(start, end))
    final def slice(range: Interval): MapTo[A]      = Sliced(this, range)
    final def labeled(label: String): MapTo[A]      = LabeledView(this, label)
    final def sized(size: Size): MapTo[A]           = Sized(this, size)
    final def reverse: MapTo[A]                     = Reversed(this)

    final def native(implicit pcb: PspCanBuild[A, Coll]): Coll      = force[Coll]
    final def force[That](implicit pcb: PspCanBuild[A, That]): That = pcb build this

    override def toString = completeString
  }

  sealed abstract class AtomicView extends View[A] with CountCalls {
    val tc: Foreachable[Coll]
    def foreachSlice(range: Interval)(f: A => Unit): Unit

    val counter: Counter = new Counter()
    def completeString = "<xs>"
    def atomicView: AtomicView = this
  }

  sealed abstract class CompositeView[+A](val description: String, val sizeEffect: SizeInfo => SizeInfo) extends CompositeViewImpl[A]

  final case class LabeledView[+A   ](prev: View[A], label: String)      extends CompositeView[A](label,            x => x)
  final case class Sized      [+A   ](prev: View[A], size: Size)         extends CompositeView[A](pp"sized $size",  _ => Precise(size))
  final case class Joined     [+A   ](prev: View[A], ys: Elemental[A])   extends CompositeView[A](pp"++ $ys",       _ + ys.sizeInfo)
  final case class Filtered   [ A   ](prev: View[A], p: A => Boolean)    extends CompositeView[A](pp"filter $p",    _.atMost)
  final case class Sliced     [+A   ](prev: View[A], range: Interval)    extends CompositeView[A](pp"slice $range", _ slice range)
  final case class Dropped    [+A   ](prev: View[A], n: Size)            extends CompositeView[A](pp"drop $n",      _ - Precise(n))
  final case class DroppedR   [+A   ](prev: View[A], n: Size)            extends CompositeView[A](pp"dropR $n",     _ - Precise(n))
  final case class Taken      [+A   ](prev: View[A], n: Size)            extends CompositeView[A](pp"take $n",      _ min Precise(n))
  final case class TakenR     [+A   ](prev: View[A], n: Size)            extends CompositeView[A](pp"takeR $n",     _ min Precise(n))
  final case class TakenWhile [ A   ](prev: View[A], p: A => Boolean)    extends CompositeView[A](pp"takeW $p",     _.atMost)
  final case class DropWhile  [ A   ](prev: View[A], p: A => Boolean)    extends CompositeView[A](pp"dropW $p",     _.atMost)
  final case class Reversed   [+A   ](prev: View[A])                     extends CompositeView[A]("reverse",        x => x)
  final case class Mapped     [ A, B](prev: View[A], f: A => B)          extends CompositeView[B](pp"map $f",       x => x)
  final case class FlatMapped [ A, B](prev: View[A], f: A => Foreach[B]) extends CompositeView[B](pp"flatMap $f",   x => if (x.isZero) x else Unknown)
  final case class Collected  [ A, B](prev: View[A], pf: A =?> B)        extends CompositeView[B](pp"collect $pf",  _.atMost)

  object FlattenIndexedSlice {
    def unapply[A](xs: View[A]): Option[(View[A], Interval)] = xs match {
      case xs: IndexedView       => Some(xs -> xs.size.toInterval)
      case LabeledView(xs, _)    => unapply(xs)
      case Sized(xs, Size(n))    => Some(xs -> Interval(0, n))
      case Mapped(xs, f)         => unapply(xs) map { case (xs, range) => (xs map f, range) }
      case DroppedR(xs, Size(n)) => unapply(xs) map { case (xs, range) => (xs, range dropRight n) }
      case TakenR(xs, Size(n))   => unapply(xs) map { case (xs, range) => (xs, range takeRight n) }
      case Dropped(xs, Size(n))  => unapply(xs) map { case (xs, range) => (xs, range drop n) }
      case Taken(xs, Size(n))    => unapply(xs) map { case (xs, range) => (xs, range take n) }
      case Sliced(xs, indices)   => unapply(xs) map { case (xs, range) => (xs, range slice indices) }
      case _                     => xs.sizeInfo.precisely map (size => xs -> Interval(0, size))
    }
  }


  sealed trait CompositeViewImpl[+A] extends View[A] {
    def prev: View[_]
    def description: String
    def sizeEffect: SizeInfo => SizeInfo

    def atomicView: AtomicView = prev.atomicView
    def sizeInfo: SizeInfo     = sizeEffect(prev.sizeInfo)

    final def foreach(f: A => Unit): Unit = {
      if (sizeInfo.isZero) return

      def loop[B](xs: Elemental[B])(f: B => Unit): Unit = xs match {
        case xs: AtomicView                           => xs foreach f
        case LabeledView(xs, _)                       => loop(xs)(f)
        case Sized(xs, size)                          => loop(xs)(f)
        case Mapped(xs, g)                            => loop(xs)(g andThen f)
        case FlatMapped(xs, g)                        => loop(xs)(x => g(x) foreach f)
        case Filtered(xs, p: Function1[B, Boolean])   => loop(xs)(x => if (p(x)) f(x))
        case TakenWhile(xs, p: Function1[B, Boolean]) => foreachTakeWhile(xs, f, p)
        case DropWhile(xs, p: Function1[B, Boolean])  => foreachDropWhile(xs, f, p)
        case Collected(xs, pf)                        => loop(xs)(x => if (pf isDefinedAt x) f(pf(x)))
        case FlattenIndexedSlice(xs, range)           => foreachSlice(xs, f, range)
        case Reversed(xs)                             => ???
        case Joined(xs, ys)                           => loop(xs)(f) ; loop(ys)(f)
        case DroppedR(xs, Size(n))                    => foreachDropRight(xs, f, Size(n))
        case TakenR(xs, Size(n))                      => foreachTakeRight(xs, f, Size(n))
        case Dropped(xs, Size(n))                     => foreachSlice(xs, f, Interval.Full drop n)
        case Taken(xs, Size(n))                       => foreachSlice(xs, f, Interval.Full take n)
        case Sliced(xs, range)                        => foreachSlice(xs, f, range)
      }
      loop(this)(f)
    }


    private def foreachSlice[A](xs: View[A], f: A => Unit, range: Interval): Unit = {
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
        case xs: AtomicView   => (xs foreachSlice range)(f)
        case Mapped(xs, g)    => foreachSlice(xs, g andThen f, range)
        case xs: Indexed[A]   => var i = range.start ; while (i < range.end) { f(xs elemAt i) ; i += 1 }
        case Joined(ys1, ys2) =>
        ys1.sizeInfo.precisely match {
          case Some(n) if n < range.start => ys2 slice (range - n) foreach f
          case Some(n) if n > range.end   => ys1 slice range foreach f
          case _                          => runThrough(ys1) || runThrough(ys2)
        }
        case _              => runThrough(xs)
      }
    }

    private def foreachTakeRight[A](xs: Foreach[A], f: A => Unit, n: Size): Unit =
      (CircularBuffer[A](n) ++= xs) foreach f

    private def foreachDropRight[A](xs: Foreach[A], f: A => Unit, n: Size): Unit =
      xs.foldl(CircularBuffer[A](n))((buf, x) => if (buf.isFull) try buf finally f(buf push x) else buf += x)

    private def foreachTakeWhile[A](xs: Foreach[A], f: A => Unit, p: A => Boolean): Unit =
      xs foreach (x => if (p(x)) f(x) else return)

    private def foreachDropWhile[A](xs: Foreach[A], f: A => Unit, p: A => Boolean): Unit = {
      var dropping = true
      xs foreach { x =>
        if (dropping && p(x)) ()
        else {
          if (dropping) dropping = false
          f(x)
        }
      }
    }

    def calls = this match {
      case Joined(xs, ys) => xs.calls + ys.calls
      case _              => prev.calls
    }

    def operationString: String = description indexOf ' ' match {
      case -1  => "%-15s" format description
      case idx => "%-7s %-7s".format(description.substring(0, idx), description.substring(idx + 1))
    }
    def completeString: String = this match {
      case LabeledView(_, label) => label
      case _                     => pp"${prev.completeString} $operationString"
    }
  }
}

