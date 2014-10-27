package psp
package std

import api._
import java.util.concurrent.SynchronousQueue
import lowlevel.CircularBuffer

object FView {
  type MapTo[X] = Foreach[X]

  trait ForView[+A] extends Foreach[A]

  final case class Split[A](left: View[A], right: View[A]) {
    def mapLeft[A1 >: A](f: Unary[View[A1]]): Split[A1]  = new Split(f(left), right)
    def mapRight[A1 >: A](f: Unary[View[A1]]): Split[A1] = new Split(left, f(right))
    def join: View[A]                                    = new Join(left, right) m
    // def intersperse: Single[A]                        = Interspersed(left, right)
    def force[That](implicit z: Builds[A, That]): That   = z build join
  }

  sealed abstract class Predicated[A](xs: MapTo[A], drop: Predicate[A], take: Predicate[A]) extends ForView[A] {
    def size = xs.size.atMost
    def foreach(f: A => Unit): Unit = {
      var dropping = true
      // Note that this looks deceptively like an if-then-else, but is not.
      // Both branches are taken when it switches from dropping to taking.
      xs foreach { x =>
        if (dropping)
          if (drop(x)) () else dropping = false

        if (!dropping)
          if (take(x)) f(x) else return
      }
    }
  }

  sealed class SlicedDirect[A](xs: Direct[A], range: IndexRange) extends ForView[A] with Direct[A] {
    def isEmpty = size.isZero
    def size    = Size.sliced(xs.size, range)
    def elemAt(index: Index): A     = xs(index + range.startInt)
    def foreach(f: A => Unit): Unit = range foreach (i => f(xs(i)))
  }

  sealed abstract class Sliced[A](xs: MapTo[A], range: IndexRange) extends ForView[A] {
    def size = xs.size slice range
    def foreach(f: A => Unit): Unit = {
      var dropping = range.precedingSize
      var taking   = range.size

      xs foreach { x =>
        if (dropping > 0)
          dropping = dropping - 1
        else if (taking > 0)
          try f(x) finally taking = taking - 1
        else
          return
      }
    }
  }
  sealed abstract class Filtered[A, B](xs: MapTo[A], p: A => Boolean, f: A => B, val size: Size) extends ForView[B] {
    def foreach(g: B => Unit): Unit = xs foreach (x => if (p(x)) g(f(x)))
  }

  final class Zip[A, B](xs: MapTo[A], ys: MapTo[B]) extends ForView[(A, B)] {
    private def spawn[A](body: => A): Unit = (new Thread() { override def run(): Unit = body }).start()

    def size = xs.size min ys.size
    def foreach(f: ((A, B)) => Unit): Unit = {
      @volatile var done = false
      val handoff = new SynchronousQueue[A]
      spawn( try xs foreach (x => if (done) return else        handoff put x) finally done = true )
           ( try ys foreach (y => if (done) return else f(handoff.take -> y)) finally done = true )
    }
  }

  sealed class Join[A](xs: MapTo[A], ys: View[A]) extends ForView[A] {
    def size = xs.size + ys.size
    @inline def foreach(f: A => Unit): Unit = {
      xs foreach f
      ys foreach f
    }
  }

  sealed class DropRight[A](xs: MapTo[A], n: Precise) extends ForView[A] {
    def size = xs.size - n
    @inline def foreach(f: A => Unit): Unit = xs.foldl(CircularBuffer[A](n))((buf, x) =>
      if (buf.isFull) try buf finally f(buf push x) else buf += x
    )
  }
  sealed class TakeRight[A](xs: MapTo[A], n: Precise) extends ForView[A] {
    def size = xs.size min n
    @inline def foreach(f: A => Unit): Unit = (CircularBuffer[A](n) ++= xs) foreach f
  }
  sealed class TakeWhile[A](xs: MapTo[A], p: Predicate[A]) extends Predicated(xs, drop = false, take = p)
  sealed class DropWhile[A](xs: MapTo[A], p: Predicate[A]) extends Predicated(xs, drop = p, take = true)
  sealed class Take[A](xs: MapTo[A], n: Precise)           extends Sliced(xs, IndexRange.full take n)
  sealed class Drop[A](xs: MapTo[A], n: Precise)           extends Sliced(xs, IndexRange.full drop n)
  sealed class Slice[A](xs: MapTo[A], range: IndexRange)   extends Sliced(xs, range)
  sealed class Map[A, B](xs: MapTo[A], f: A => B)          extends Filtered(xs, true, f, xs.size)
  sealed class Collect[A, B](xs: MapTo[A], pf: A ?=> B)    extends Filtered[A, B](xs, pf isDefinedAt _, pf, xs.size.atMost)
  sealed class Filter[A](xs: MapTo[A], p: Predicate[A])    extends Filtered(xs, p, identity[A], xs.size.atMost)
  sealed class Identity[A](xs: MapTo[A])                   extends Filtered(xs, true, identity[A], xs.size)

  final class FlatMap[A, B](xs: MapTo[A], f: A => View[B]) extends ForView[B] {
    def size = Size.unknown
    def foreach(g: B => Unit): Unit = xs foreach (x => f(x) foreach g)
  }
}
