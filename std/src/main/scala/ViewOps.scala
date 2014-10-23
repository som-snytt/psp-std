package psp
package std
package ops

import api._

final class WeakApiViewOps[A](val xs: View[A]) {
  def chainDescriptions: pVector[String]               = xs.viewChain.reverse collect { case x: BaseView[_,_] => x } map (_.description)
  def grep(regex: Regex)(implicit z: Show[A]): View[A] = xs filter (x => regex isMatch x)

  // def drop(n: Int): xs.MapTo[A]      = xs drop n.size
  // def take(n: Int): xs.MapTo[A]      = xs take n.size
  // def dropRight(n: Int): xs.MapTo[A] = xs dropRight n.size
  // def takeRight(n: Int): xs.MapTo[A] = xs takeRight n.size
}

class BaseViewOps[A, Repr](xs: BaseView[A, Repr]) {
  def tail: BaseView[A, Repr]                      = xs drop 1
  def init: BaseView[A, Repr]                      = xs dropRight 1
  def mapWithIndex[B](f: (A, Index) => B): View[B] = Foreach[B](mf => xs.foldl(0)((res, x) => try res + 1 finally mf(f(x, Index(res))))).m[Foreachable]
}
