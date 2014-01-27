package psp
package compat

import psp.core._

/** Compatibility layer for wrapping scala views on their own terms.
 */
final class ScalaNative[+A](val xs: Iterable[A], val counter: Counter) extends ElementalView[A] with CountCalls {
  type MapTo[+X] = ScalaNative[X]

  private implicit def lift[B](result: Iterable[B]): MapTo[B] = new ScalaNative(result, counter)

  def sizeInfo: SizeInfo = xs match {
    case xs: IndexedSeq[_] => precise(xs.size)
    case _                 => SizeInfo.Unknown
  }
  def foreach(f: A => Unit): Unit               = xs foreach f
  def map[B](f: A => B): MapTo[B]               = xs map f
  def flatMap[B](f: A => Foreach[B]): MapTo[B]  = xs flatMap (x => f(x).toTraversable.seq)
  def ++[A1 >: A](that: Foreach[A1]): MapTo[A1] = xs ++ that.toTraversable.seq
  def filter(p: A => Boolean): MapTo[A]         = xs filter p
  def slice(start: Int, end: Int): MapTo[A]     = xs.slice(start, end)
  def drop(n: Int): MapTo[A]                    = xs drop n
  def take(n: Int): MapTo[A]                    = xs take n
  def dropRight(n: Int): MapTo[A]               = xs dropRight n
  def takeRight(n: Int): MapTo[A]               = xs takeRight n
  def toForeach: Foreach[A]                     = Foreach(this foreach _)

  def collect[B](pf: PartialFunction[A,B]): MapTo[B] = xs collect pf
  def labeled(label: String): MapTo[A]               = this
  def reverse: MapTo[A]                              = xs.reverse
  def sized(size: psp.core.Size): MapTo[A]           = this
  def slice(range: psp.core.Interval): MapTo[A]      = xs.slice(range.start, range.end)
  def withFilter(p: A => Boolean): MapTo[A]          = xs filter p

  override def toString = xs.shortClass
}

object ScalaNative {
  def apply[A](xs: Iterable[A]): ScalaNative[A] = {
    val counter = new Counter()
    new ScalaNative(xs map counter.record, counter)
  }
  def unapply[A](n: ScalaNative[A]) = Some(n.xs -> n.counter)
}
