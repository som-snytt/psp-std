package psp
package compat

import psp.std._

/** Compatibility layer for wrapping scala views on their own terms.
 */
final class ScalaNative[+A](val xs: Iterable[A], val counter: Counter) extends api.View[A] with CountCalls {
  type MapTo[+X] = ScalaNative[X]

  private implicit def lift[B](result: Iterable[B]): MapTo[B] = new ScalaNative(result, counter)

  def ++[A1 >: A](that: Foreach[A1]): MapTo[A1]      = xs ++ that.trav
  def collect[B](pf: PartialFunction[A,B]): MapTo[B] = xs collect pf
  def description                                    = xs.shortClass
  def drop(n: Int): MapTo[A]                         = xs drop n
  def dropRight(n: Int): MapTo[A]                    = xs dropRight n
  def dropWhile(p: Predicate[A]): MapTo[A]           = xs dropWhile p
  def filter(p: Predicate[A]): MapTo[A]              = xs filter p
  def filterNot(p: Predicate[A]): MapTo[A]           = xs filterNot p
  def flatMap[B](f: A => Foreach[B]): MapTo[B]       = xs flatMap (x => f(x).trav)
  def foreach(f: A => Unit): Unit                    = xs foreach f
  def labeled(label: String): MapTo[A]               = this
  def map[B](f: A => B): MapTo[B]                    = xs map f
  def sizeInfo: SizeInfo                             = xs maybe { case xs: IndexedSeq[_] => SizeInfo(xs.size) } or unknownSize
  def sized(size: api.Size): MapTo[A]                = this
  def slice(range: IndexRange): MapTo[A]             = xs.slice(range.startInt, range.endInt)
  def take(n: Int): MapTo[A]                         = xs take n
  def takeRight(n: Int): MapTo[A]                    = xs takeRight n
  def takeWhile(p: Predicate[A]): MapTo[A]           = xs takeWhile p
  def viewChain: List[api.View[_]]                   = this :: Nil
  def withFilter(p: Predicate[A]): MapTo[A]          = xs filter p

  override def toString = description
}

object ScalaNative {
  def apply[A](xs: Iterable[A]): ScalaNative[A] = {
    val counter = new Counter()
    new ScalaNative(xs map counter.record, counter)
  }
  def unapply[A](n: ScalaNative[A]) = Some(n.xs -> n.counter)
}
