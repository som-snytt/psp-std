package psp
package compat

import psp.std._, api._

/** Compatibility layer for wrapping scala views on their own terms.
 */
final class ScalaNative[+A](val xs: sIterable[A], val counter: RecorderCounter) extends View[A] with CountCalls with RearSliceable[ScalaNative[A]] {
  type MapTo[+X] = ScalaNative[X]

  private implicit def lift[B](result: sIterable[B]): MapTo[B] = new ScalaNative(result, counter)
  private implicit def lower[A](x: View[A]): scTraversable[A]  = BiIterable(x)

  def ++[A1 >: A](that: View[A1]): MapTo[A1]   = xs ++ that
  def collect[B](pf: A ?=> B): MapTo[B]        = xs collect pf
  def description: String                      = xs.shortClass
  def drop(n: PreciseSize): MapTo[A]           = xs drop n.intSize
  def dropRight(n: PreciseSize): MapTo[A]      = xs dropRight n.intSize
  def dropWhile(p: Predicate[A]): MapTo[A]     = xs dropWhile p
  def filter(p: Predicate[A]): MapTo[A]        = xs filter p
  def filterNot(p: Predicate[A]): MapTo[A]     = xs filterNot p
  def flatMap[B](f: A => Foreach[B]): MapTo[B] = xs flatMap (x => BiIterable(f(x)))
  def foreach(f: A => Unit): Unit              = xs foreach f
  def labeled(label: String): MapTo[A]         = this
  def map[B](f: A => B): MapTo[B]              = xs map f
  def sizeInfo: SizeInfo                       = SizeInfo(xs)
  def sized(size: PreciseSize): MapTo[A]       = this
  def slice(range: IndexRange): MapTo[A]       = xs.slice(range.startInt, range.endInt)
  def take(n: PreciseSize): MapTo[A]           = xs take n.intSize
  def takeRight(n: PreciseSize): MapTo[A]      = xs takeRight n.intSize
  def takeWhile(p: Predicate[A]): MapTo[A]     = xs takeWhile p
  def viewChain: Foreach[View[_]]              = newSeq(this)
  def viewRange: IndexRange                    = indexRange(0, xs.size)
  def withFilter(p: Predicate[A]): MapTo[A]    = xs filter p  // scala withFilter returns "FilterMonadic"

  override def toString = description
}

object ScalaNative {
  def apply[A](xs: sIterable[A]): ScalaNative[A] = new RecorderCounter() |> (c => new ScalaNative(xs map c.record, c))
  def unapply[A](n: ScalaNative[A])              = Some(n.xs -> n.counter)
}
