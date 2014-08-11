package psp
package compat

import psp.core._
import psp.std.IndexRange

/** Compatibility layer for wrapping scala views on their own terms.
 */
final class ScalaNative[+A](val xs: Iterable[A], val counter: Counter) extends api.View[A] with CountCalls {
  type MapTo[+X] = ScalaNative[X]

  // TODO:
  // type Input[X]  = GenTraversableOnce[X]
  // def flatMap[B](f: A => Input[B]): MapTo[B]            = xs flatMap f
  // def flatten[B](implicit ev: A <:< Input[B]): MapTo[B] = xs flatMap ev

  private implicit def lift[B](result: Iterable[B]): MapTo[B] = new ScalaNative(result, counter)

  def sizeInfo: SizeInfo = xs match {
    case xs: IndexedSeq[_] => Size(xs.size)
    case _                 => unknownSize
  }
  def foreach(f: A => Unit): Unit                       = xs foreach f
  def map[B](f: A => B): MapTo[B]                       = xs map f
  def ++[A1 >: A](that: Foreach[A1]): MapTo[A1]         = xs ++ that.toTraversable.seq
  def flatMap[B](f: A => Input[B]): MapTo[B]            = xs flatMap (x => f(x).trav)
  def flatten[B](implicit ev: A <:< Input[B]): MapTo[B] = xs flatMap (x => ev(x).trav)
  def filter(p: Predicate[A]): MapTo[A]                 = xs filter p
  def filterNot(p: Predicate[A]): MapTo[A]              = xs filterNot p
  def drop(n: Int): MapTo[A]                            = xs drop n
  def take(n: Int): MapTo[A]                            = xs take n
  def takeWhile(p: Predicate[A]): MapTo[A]              = xs takeWhile p
  def dropWhile(p: Predicate[A]): MapTo[A]              = xs dropWhile p
  def dropRight(n: Int): MapTo[A]                       = xs dropRight n
  def takeRight(n: Int): MapTo[A]                       = xs takeRight n

  def collect[B](pf: PartialFunction[A,B]): MapTo[B] = xs collect pf
  def labeled(label: String): MapTo[A]               = this
  def reverse: MapTo[A]                              = xs.reverse
  def sized(size: Size): MapTo[A]                    = this
  def slice(range: IndexRange): MapTo[A]             = xs.slice(range.start, range.end)
  def withFilter(p: Predicate[A]): MapTo[A]          = xs filter p

  def description = xs.shortClass
  def viewChain: List[api.View[_]] = this :: Nil
  override def toString = description
}

object ScalaNative {
  def apply[A](xs: Iterable[A]): ScalaNative[A] = {
    val counter = new Counter()
    new ScalaNative(xs map counter.record, counter)
  }
  def unapply[A](n: ScalaNative[A]) = Some(n.xs -> n.counter)
}
