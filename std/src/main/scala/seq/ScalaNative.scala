package psp
package compat

import psp.std._, api._

/** Compatibility layer for wrapping scala views on their own terms.
 */
final class ScalaNative[+A](val xs: sIterable[A], val counter: RecorderCounter) extends View[A] with CountCalls {
  type MapTo[+X] = ScalaNative[X]

  private implicit def lift[B](result: sIterable[B]): MapTo[B]    = new ScalaNative(result, counter)
  private implicit def lower[A](x: api.View[A]): scTraversable[A] = BiIterable(x)

  def ++[A1 >: A](that: View[A1]): MapTo[A1]          = xs ++ that
  def collect[B](pf: PartialFunction[A,B]): MapTo[B]  = xs collect pf
  def count(p: Predicate[A]): Precise                 = Precise(xs count p)
  def description                                     = xs.shortClass
  def drop(n: Int): MapTo[A]                          = xs drop n
  def dropRight(n: Int): MapTo[A]                     = xs dropRight n
  def dropWhile(p: Predicate[A]): MapTo[A]            = xs dropWhile p
  def filter(p: Predicate[A]): MapTo[A]               = xs filter p
  def filterNot(p: Predicate[A]): MapTo[A]            = xs filterNot p
  def flatMap[B](f: A => Foreach[B]): MapTo[B]        = xs flatMap (x => BiIterable(f(x)))
  def foreach(f: A => Unit): Unit                     = xs foreach f
  def labeled(label: String): MapTo[A]                = this
  def map[B](f: A => B): MapTo[B]                     = xs map f
  def sizeInfo: SizeInfo                              = xs.matchOr(SizeInfo.unknown) { case xs: IndexedSeq[_] => SizeInfo(xs.size) }
  def sized(size: Size): MapTo[A]                     = this
  def slice(range: IndexRange): MapTo[A]              = xs.slice(range.start.indexValue, range.end.indexValue)
  def take(n: Int): MapTo[A]                          = xs take n
  def takeRight(n: Int): MapTo[A]                     = xs takeRight n
  def takeWhile(p: Predicate[A]): MapTo[A]            = xs takeWhile p
  def viewChain: Foreach[View[_]]                     = Foreach elems this
  def viewRange                                       = IndexRange(0, xs.size)
  def withFilter(p: Predicate[A]): MapTo[A]           = xs filter p
  // def zipWith[B, C](that: View[B])(f: (A, B) => C) = xs zip that.toIterable map { case (x, y) => f(x, y) }

  override def toString = description
}

object ScalaNative {
  def apply[A](xs: sIterable[A]): ScalaNative[A] = new RecorderCounter() |> (c => new ScalaNative(xs map c.record, c))
  def unapply[A](n: ScalaNative[A])              = Some(n.xs -> n.counter)
}
