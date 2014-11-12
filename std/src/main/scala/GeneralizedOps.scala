package psp
package std
package ops

import api._

/** Experiments in making operations more uniform.
 */

sealed trait Stretch[A]
object Stretch {
  implicit def stretchSize[A](s: Precise): Stretch[A]                   = Distance(s)
  implicit def stretchCondition[A](p: Predicate2[A, Index]): Stretch[A] = Condition(p)

  /** This is investigating unifying take, takeWhile, drop, dropWhile, span, splitAt
   *  into a single entrypoint.
   */
  final case class Distance[A](size: Precise) extends Stretch[A]
  final case class Condition[A](p: Predicate2[A, Index]) extends Stretch[A]
}

/** If we user the initial value as a jumping-off point can we improve the
 *  interface to folds?
 */
trait FoldOps[+A, B] extends Any {
  def xs: View[A]
  def initial: B

  def indexed(f: (B, A, Index) => B): B = {
    var res = initial
    xs.zipIndex foreach ((x, i) => res = f(res, x, i))
    res
  }
  def left(f: (B, A) => B): B = {
    var res = initial
    xs foreach (x => res = f(res, x))
    res
  }
  def right(f: (A, B) => B): B = {
    var res = initial
    xs foreachReverse (x => res = f(x, res))
    res
  }
}
final case class FoldOpsClass[+A, B](xs: View[A], initial: B) extends FoldOps[A, B]

trait FoldViewOps[A] extends Any with ApiViewOps[A] {
  def gtake(s: Stretch[A]): View[A] = s match {
    case Stretch.Distance(size) => xs take size
    case Stretch.Condition(p)   => zipIndex.pairs takeWhile p.tupled map fst
  }
  def gdrop(s: Stretch[A]): View[A] = s match {
    case Stretch.Distance(size) => xs drop size
    case Stretch.Condition(p)   => zipIndex.pairs dropWhile p.tupled map fst
  }
}
