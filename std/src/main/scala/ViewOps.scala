package psp
package std
package ops

import api._

final class WeakApiViewOps[A](val xs: View[A]) {
  def isEmpty: Boolean = andTrue(xs foreach (_ => return false))
  def head: A          = xs take      1 optionally { case PSeq(x) => x } orFail "empty.head"
  def last: A          = xs takeRight 1 optionally { case PSeq(x) => x } orFail "empty.last"
  def tail: View[A]    = xs drop      1
  def init: View[A]    = xs dropRight 1

  def boundedClosure(maxDepth: Precise, f: A => View[A]): View[A] =
    if (maxDepth.isZero) xs
    else (xs flatMap f) |> (ys => xs ++ ys.boundedClosure(maxDepth - 1, f))

  def chainDescriptions: pVector[String]               = xs.viewChain.reverse collect { case x: BaseView[_,_] => x } map (_.description)
  def grep(regex: Regex)(implicit z: Show[A]): View[A] = xs filter (x => regex isMatch x)
}

class BaseViewOps[A, Repr](xs: BaseView[A, Repr]) {
  def tail: BaseView[A, Repr]                                     = xs drop 1
  def init: BaseView[A, Repr]                                     = xs dropRight 1
  def mapWithIndex[B](f: (A, Index) => B): View[B]                = Foreach[B](mf => xs.foldl(0)((res, x) => try res + 1 finally mf(f(x, Index(res))))).m[Foreachable]
}

final class PairViewOps[R, A, B](val xs: View[R])(implicit paired: PairDown[R, A, B]) {
  // We should be able to write these method on the normal ViewOps and take the implicit
  // parameter here, like this:
  //   def mapPairs[B, C, D](f: (B, C) => D)(implicit z: Paired[A, B, C]): View[D] = xs map (x => f(z left x, z right x))
  // But scala's type inference sucks so it doesn't work without annotating the parameter types.
  def mapPairs[C](f: (A, B) => C): View[C]                                  = xs map (x => f(paired left x, paired right x))
  def mapLeft[R1, A1](f: A => A1)(implicit z: PairUp[R1, A1, B]): View[R1]  = xs map (x => z.create(f(paired left x), paired right x))
  def mapRight[R1, B1](f: B => B1)(implicit z: PairUp[R1, A, B1]): View[R1] = xs map (x => z.create(paired left x, f(paired right x)))
}
