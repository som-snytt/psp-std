package psp
package std
package ops

import api._

class WeakApiViewOps[A](xs: View[A]) {
  def chainDescriptions: pVector[String]               = xs.viewChain.reverse collect { case x: BaseView[_,_] => x } map (_.description)
  def grep(regex: Regex)(implicit z: Show[A]): View[A] = xs filter (x => regex isMatch x)
}

class BaseViewOps[A, Repr](xs: BaseView[A, Repr]) {
  def tail: BaseView[A, Repr]                           = xs drop 1
  def init: BaseView[A, Repr]                           = xs dropRight 1
  def mapWithIndex[B](f: (A, Index) => B): View[B]      = Foreach[B](mf => xs.foldl(0)((res, x) => try res + 1 finally mf(f(x, Index(res))))).m[Foreachable]
}
