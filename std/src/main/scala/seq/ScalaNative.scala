package psp
package compat

import psp.std._, api._
import ScalaNative._

/** Compatibility layer for wrapping scala views on their own terms.
 */
final class ScalaNative[+A] private (val xs: scIterable[A], val counter: RecorderCounter) extends View[A] {
  type MapTo[+X]   = ScalaNative[X]
  type SplitTo[+X] = ScalaNative.Split[X]

  private implicit def lift[B](result: scIterable[B]): MapTo[B]           = new ScalaNative(result, counter)
  private implicit def liftPair[B](xs: PairOf[scIterable[B]]): SplitTo[B] = ScalaNative.Split(lift(xs._1) -> lift(xs._2))
  private implicit def lower[A](x: View[A]): scIterable[A]                = new Each.ToScala(x)

  def ++[A1 >: A](that: View[A1]): MapTo[A1]          = xs ++ that
  def collect[B](pf: A ?=> B): MapTo[B]               = xs collect pf
  def drop(n: Precise): MapTo[A]                      = xs drop n.intSize
  def dropRight(n: Precise): MapTo[A]                 = xs dropRight n.intSize
  def dropWhile(p: Predicate[A]): MapTo[A]            = xs dropWhile p
  def flatMap[B](f: A => Each[B]): MapTo[B]           = xs flatMap (x => BiIterable(f(x)))
  def foreach(f: A => Unit): Unit                     = xs foreach f
  def intersperse[A1 >: A](that: View[A1]): MapTo[A1] = ??? // TODO
  def map[B](f: A => B): MapTo[B]                     = xs map f
  def size: IntSize                                   = Precise(xs.size)
  def take(n: Precise): MapTo[A]                      = xs take n.intSize
  def takeRight(n: Precise): MapTo[A]                 = xs takeRight n.intSize
  def takeWhile(p: Predicate[A]): MapTo[A]            = xs takeWhile p
  def viewOps                                         = Direct(toString)
  def withFilter(p: Predicate[A]): MapTo[A]           = xs filter p  // scala withFilter returns "FilterMonadic"
  def zip[B](that: View[B]): MapTo[(A, B)]            = xs zip that

  def dropIndex(index: Index): MapTo[A]      = splitAt(index) mapRight (_ drop 1) join
  def splitAt(index: Index): SplitTo[A]      = xs splitAt index.safeToInt
  def span(p: Predicate[A]): SplitTo[A]      = xs span p
  def partition(p: Predicate[A]): SplitTo[A] = xs partition p

  override def toString = xs.shortClass
}

object ScalaNative {
  final case class Split[+A](pair: PairOf[ScalaNative[A]]) extends View.Split[A] {
    type Single[+X]  = ScalaNative[X]

    def left                                                    = pair._1
    def right                                                   = pair._2
    def mapLeft[A1 >: A](f: Unary[ScalaNative[A1]]): Split[A1]  = Split(f(left) -> right)
    def mapRight[A1 >: A](f: Unary[ScalaNative[A1]]): Split[A1] = Split(left -> f(right))
    def join: ScalaNative[A]                                    = left ++ right
    def intersperse: ScalaNative[A]                             = ??? // TODO
    def force[That](implicit z: Builds[A, That]): That          = z build join
  }

  def apply[A](xs: scIterable[A]): ScalaNative[A] = new RecorderCounter() |> (c => new ScalaNative(xs map c.record, c))
  def unapply[A](n: ScalaNative[A])               = Some(n.xs -> n.counter)
}
