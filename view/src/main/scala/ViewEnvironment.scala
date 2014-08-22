package psp
package core

import psp.std._
import SizeInfo._

object AtomicView {
  def linear[Repr](repr: Repr)(implicit tc: SequentialAccess[Repr]): LinearView[Repr, tc.type]   = new Env[Repr, tc.type](repr) linearView tc
  def indexed[Repr](repr: Repr)(implicit tc: DirectAccess[Repr]): IndexedView[Repr, tc.type]  = new Env[Repr, tc.type](repr) indexedView tc
  def unknown[Repr](repr: Repr)(implicit tc: Foreachable[Repr]): AtomicView[Repr, tc.type] = new Env[Repr, tc.type](repr) unknownView tc
}

class ViewEnvironment[A0, Repr, CC0[X]](val repr: Repr) {
  type A = A0
  type CC[X] = CC0[X]

  def linearView(tc: SequentialAccessType[A, Repr, CC]): LinearView    = new LinearView(tc)
  def indexedView(tc: DirectAccessType[A, Repr, CC]): IndexedView   = new IndexedView(tc)
  def unknownView(tc: ForeachableType[A, Repr, CC]): UnknownView = new UnknownView(tc)

  final class UnknownView(val tc: ForeachableType[A, Repr, CC]) extends AtomicView with LinearViewImpls {
    def sizeInfo = unknownSize
    @inline def foreach(f: A => Unit): Unit = foreachSlice(IndexRange.full)(f)
  }

  final class LinearView(val tc: SequentialAccessType[A, Repr, CC]) extends AtomicView with Linear[A] with LinearViewImpls {
    type Tail = psp.core.LinearView[Repr, tc.type]

    def isEmpty    = tc isEmpty repr
    def head: A    = recordCall(tc head repr)
    def tail: Tail = tc wrap (tc tail repr)
    def sizeInfo   = if (isEmpty) Empty else NonEmpty

    @inline def foreach(f: A => Unit): Unit = foreachSlice(IndexRange.full)(f)
  }

  final class IndexedView(val tc: DirectAccessType[A, Repr, CC]) extends AtomicView with IndexedLeafImpl[A] {
    def isDefinedAt(index: Index): Boolean                  = size containsIndex index
    def size: Size                                          = tc length repr
    def elemAt(index: Index): A                             = recordCall(tc.elemAt(repr)(index))
    def contains(x: A): Boolean                             = this exists (_ == x)
    def foreach(f: A => Unit): Unit                         = foreachSlice(size.toIndexRange)(f)
    def foreachSlice(range: IndexRange)(f: A => Unit): Unit = range foreach (i => f(elemAt(i)))
  }

  sealed trait View[+A] extends Any with api.BuilderView[A, Repr] {
    type MapTo[+X] = View[X]
    // Eventually
    // type Input[X]  = Foreach[X]

    def isAtomic: Boolean

    final def map[B](f: A => B): MapTo[B]                       = Mapped(this, f)
    final def flatMap[B](f: A => Foreach[B]): MapTo[B]          = FlatMapped(this, f)
    final def flatten[B](implicit ev: A <:< Input[B]): MapTo[B] = flatMap(x => x)
    final def collect[B](pf: A ?=> B): MapTo[B]                 = Collected(this, pf)
    final def ++[A1 >: A](that: Foreach[A1]): MapTo[A1]         = Joined(this, that.m.castTo[View[A1]])

    final def withFilter(p: Predicate[A]): MapTo[A] = Filtered(this, p)
    final def filter(p: Predicate[A]): MapTo[A]     = Filtered(this, p)
    final def filterNot(p: Predicate[A]): MapTo[A]  = Filtered(this, (x: A) => !p(x))
    final def drop(n: Int): MapTo[A]                = Dropped(this, Size(n))
    final def take(n: Int): MapTo[A]                = Taken(this, Size(n))
    final def takeWhile(p: Predicate[A]): MapTo[A]  = TakenWhile(this, p)
    final def dropWhile(p: Predicate[A]): MapTo[A]  = DropWhile(this, p)
    final def dropRight(n: Int): MapTo[A]           = DroppedR(this, Size(n))
    final def takeRight(n: Int): MapTo[A]           = TakenR(this, Size(n))
    final def slice(range: IndexRange): MapTo[A]    = Sliced(this, range)
    final def labeled(label: String): MapTo[A]      = LabeledView(this, label)
    final def sized(size: Size): MapTo[A]           = Sized(this, size)
    final def reverse: MapTo[A]                     = Reversed(this)

    final def native(implicit pcb: Builds[A, Repr]): Repr      = force[Repr]
    final def force[That](implicit pcb: Builds[A, That]): That = pcb build this

    override def toString = viewChain reverseMap (_.description) mkString " "
  }

  trait LinearViewImpls {
    self: AtomicView =>

    final def foreachSlice(range: IndexRange)(f: tc.A => Unit): Unit = {
      var i = psp.std.Index.zero
      (tc foreach repr) { x =>
        recordCall(x)
        if (range contains i) f(x)
        i = i.next
        if (i >= range.end) return
      }
    }
  }

  sealed abstract class AtomicView extends api.AtomicView[A] with View[A] with CountCalls {
    val tc: Walkable[Repr]
    def foreachSlice(range: IndexRange)(f: A => Unit): Unit

    val counter: Counter = new Counter()
    final def m: this.type = this

    def description: String = ""
    def isAtomic = true
    def viewChain: List[api.View[_]] = this :: Nil
  }

  sealed abstract class CompositeView[+A](val description: String, val sizeEffect: SizeInfo => SizeInfo) extends api.CompositeView[A] with CompositeViewImpl[A] {
    def isAtomic = false
  }

  final case class LabeledView[+A   ](prev: api.View[A], label: String)      extends CompositeView[A](label,            x => x)
  final case class Sized      [+A   ](prev: api.View[A], size: Size)         extends CompositeView[A](pp"sized $size",  _ => size)
  final case class Joined     [+A   ](prev: api.View[A], ys: api.View[A])    extends CompositeView[A](pp"++ $ys",       _ + ys.sizeInfo)
  final case class Filtered   [ A   ](prev: api.View[A], p: Predicate[A])    extends CompositeView[A](pp"filter $p",    _.atMost)
  final case class Sliced     [+A   ](prev: api.View[A], range: IndexRange)  extends CompositeView[A](pp"slice $range", _ slice range)
  final case class Dropped    [+A   ](prev: api.View[A], n: Size)            extends CompositeView[A](pp"drop $n",      _ - n)
  final case class DroppedR   [+A   ](prev: api.View[A], n: Size)            extends CompositeView[A](pp"dropR $n",     _ - n)
  final case class Taken      [+A   ](prev: api.View[A], n: Size)            extends CompositeView[A](pp"take $n",      _ min n)
  final case class TakenR     [+A   ](prev: api.View[A], n: Size)            extends CompositeView[A](pp"takeR $n",     _ min n)
  final case class TakenWhile [ A   ](prev: api.View[A], p: Predicate[A])    extends CompositeView[A](pp"takeW $p",     _.atMost)
  final case class DropWhile  [ A   ](prev: api.View[A], p: Predicate[A])    extends CompositeView[A](pp"dropW $p",     _.atMost)
  final case class Reversed   [+A   ](prev: api.View[A])                     extends CompositeView[A]("reverse",        x => x)
  final case class Mapped     [ A, B](prev: api.View[A], f: A => B)          extends CompositeView[B](pp"map $f",       x => x)
  final case class FlatMapped [ A, B](prev: api.View[A], f: A => Foreach[B]) extends CompositeView[B](pp"flatMap $f",   x => if (x.isZero) x else Unknown)
  final case class Collected  [ A, B](prev: api.View[A], pf: A ?=> B)        extends CompositeView[B](pp"collect $pf",  _.atMost)

  object FlattenIndexedSlice {
    def unapply[A](xs: api.View[A]): Option[(api.View[A], IndexRange)] = xs match {
      case xs: IndexedView       => Some(xs -> xs.size.toIndexRange)
      case LabeledView(xs, _)    => unapply(xs)
      case Sized(xs, Size(n))    => Some(xs -> indexRange(0, n))
      case Mapped(xs, f)         => unapply(xs) map { case (xs, range) => (xs map f, range) }
      case DroppedR(xs, Size(n)) => unapply(xs) map { case (xs, range) => (xs, range dropRight n) }
      case TakenR(xs, Size(n))   => unapply(xs) map { case (xs, range) => (xs, range takeRight n) }
      case Dropped(xs, Size(n))  => unapply(xs) map { case (xs, range) => (xs, range drop n) }
      case Taken(xs, Size(n))    => unapply(xs) map { case (xs, range) => (xs, range take n) }
      case Sliced(xs, indices)   => unapply(xs) map { case (xs, range) => (xs, range slice indices) }
      case _                     => xs.sizeInfo.precisely map (size => xs -> indexRange(0, size))
    }
  }


  sealed trait CompositeViewImpl[+A] extends View[A] {
    def prev: api.View[_]
    def viewChain: List[api.View[_]] = this :: prev.viewChain
    def sizeEffect: SizeInfo => SizeInfo

    def sizeInfo: SizeInfo     = sizeEffect(prev.sizeInfo)

    final def foreach(f: A => Unit): Unit = {
      if (sizeInfo.isZero) return

      def loop[B](xs: api.View[B])(f: B => Unit): Unit = xs match {

        case LabeledView(xs, _)              => loop[B](xs)(f)
        case Sized(xs, size)                 => loop[B](xs)(f)
        // wartremover informs me the loop type argument is inferred as Any!
        // To the extent that scala is sound, it is by accident.
        // case Mapped(xs, g)                   => loop(xs)(g andThen f)
        // case FlatMapped(xs, g)               => loop(xs)(x => g(x) foreach f)
        case m: Mapped[a, _]                 => loop[a](m.prev)(m.f andThen f)
        case m: FlatMapped[a, _]             => loop[a](m.prev)(x => m.f(x) foreach f)
        case Filtered(xs, p: Predicate[B])   => loop(xs)(x => if (p(x)) f(x))
        case TakenWhile(xs, p: Predicate[B]) => foreachTakeWhile(xs, f, p)
        case DropWhile(xs, p: Predicate[B])  => foreachDropWhile(xs, f, p)
        case Collected(xs, pf)               => loop(xs)(x => if (pf isDefinedAt x) f(pf(x)))
        case FlattenIndexedSlice(xs, range)  => foreachSlice(xs, f, range)
        case Reversed(xs)                    => ???
        case Joined(xs, ys)                  => loop(xs)(f) ; loop(ys)(f)
        case DroppedR(xs, Size(n))           => foreachDropRight(xs, f, Size(n))
        case TakenR(xs, Size(n))             => foreachTakeRight(xs, f, Size(n))
        case Dropped(xs, Size(n))            => foreachSlice(xs, f, IndexRange.full drop n)
        case Taken(xs, Size(n))              => foreachSlice(xs, f, IndexRange.full take n)
        case Sliced(xs, range)               => foreachSlice(xs, f, range)
        case xs: api.View[_]                 => xs foreach f
        case xs                              => sys.error(pp"Unexpected view class ${xs.shortClass}")
      }
      loop(this)(f)
    }

    private def foreachSlice[A](xs: api.View[A], f: A => Unit, range: IndexRange): Unit = {
      var i = Index.zero
      def isDone = range.end <= i
      def runThrough(xs: Foreach[A]): Unit = {
        xs foreach { x =>
          if (range contains i) f(x)
          i = i.next
          if (isDone) return
        }
      }

      xs match {
        case xs: AtomicView   => (xs foreachSlice range)(f)
        case m: Mapped[a, _]  => foreachSlice[a](m.prev, m.f andThen f, range)
        case xs: Direct[A]    => range foreach (i => f(xs(i)))
        case Joined(ys1, ys2) =>
          ys1.sizeInfo match {
            case Precise(n) if n.lastIndex < range.start => ys2 slice (range << n.toInt) foreach f
            case Precise(n) if n.lastIndex > range.end   => ys1 slice range foreach f
            case _                                       => runThrough(ys1) ; if (!isDone) runThrough(ys2)
          }
        case _              => runThrough(xs)
      }
    }

    private def foreachTakeRight[A](xs: Foreach[A], f: A => Unit, n: Size): Unit =
      (CircularBuffer[A](n) ++= xs) foreach f

    private def foreachDropRight[A](xs: Foreach[A], f: A => Unit, n: Size): Unit =
      xs.foldl(CircularBuffer[A](n))((buf, x) => if (buf.isFull) try buf finally f(buf push x) else buf += x)

    private def foreachTakeWhile[A](xs: Foreach[A], f: A => Unit, p: Predicate[A]): Unit =
      xs foreach (x => if (p(x)) f(x) else return)

    private def foreachDropWhile[A](xs: Foreach[A], f: A => Unit, p: Predicate[A]): Unit = {
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
  }
}
