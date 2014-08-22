package psp
package core

import impl._
import psp.std._

trait CollectionLow {
  // aka AtomicView[Repr, tc.type] but SI-8223. Similarly for the analogous implicits.
  implicit def raiseAtomicView[Repr](repr: Repr)(implicit tc: Foreachable[Repr]): Env[Repr, tc.type]#AtomicView = tc wrap repr
}
trait CollectionMid extends CollectionLow {
  implicit def raiseSequentialAccessView[Repr](repr: Repr)(implicit tc: SequentialAccess[Repr]): Env[Repr, tc.type]#LinearView = tc wrap repr
}
trait CollectionHigh extends CollectionMid {
  implicit def raiseIndexedView[Repr](repr: Repr)(implicit tc: DirectAccess[Repr]): Env[Repr, tc.type]#IndexedView = tc wrap repr
}

trait PspUniversals {
  @inline final implicit def raisePspViewInt(x: Int): PspViewInt = new PspViewInt(x)
}

final class PspViewInt(val self: Int) extends AnyVal {
  def until(end: Int): IntRange        = IntRange.until(self, end)
  def to(end: Int): IntRange           = IntRange.to(self, end)
  def times[A](body: => A): Foreach[A] = Direct.fill(self)(body)
}

trait PspLowPriority extends PspUniversals {
  // See lowerNativeView.
  implicit def lowerGenericView[A, B, CC[X]](xs: ViewEnvironment[A, _, CC]#View[B])(implicit pcb: Builds[B, CC[B]]): CC[B] = xs.force
}

trait PspMidPriority extends PspLowPriority {
  implicit def raisePspStringOps(s: String): PspStringOps = new PspStringOps(s)
  implicit def lowerPspStringOps(s: PspStringOps): String = s.repr

  // I don't think this should be implicit, but people are so easily impressed
  // and so easily latch onto irrelevant details, we are sort of forced to be
  // gimmick-compatible with scala to silence them.
  implicit def lowerNativeView[A, Repr, CC[X]](xs: ViewEnvironment[A, Repr, CC]#View[A])(implicit pcb: Builds[A, Repr]): Repr = xs.native
}

trait PspHighPriority extends PspMidPriority with CollectionHigh {
  implicit def raisePartialFunctionOps[T, R](pf: T ?=> R): PartialFunctionOps[T, R]                                                = new PartialFunctionOps[T, R](pf)
  implicit def raiseFunction1Ops[T, R](f: T => R): Function1Ops[T, R]                                                              = new Function1Ops[T, R](f)
  implicit def raiseFunction2Ops[T1, T2, R](f: (T1, T2) => R): Function2Ops[T1, T2, R]                                             = new Function2Ops(f)
  implicit def raiseExtraViewOps[A, B, Repr, CC[X]](xs: ViewEnvironment[A, Repr, CC]#View[B]): ExtraViewOperations[A, B, Repr, CC] = new ExtraViewOperations[A, B, Repr, CC](xs)

  implicit def convertCanBuildFrom[Elem, To](implicit cbf: CanBuildFrom[_, Elem, To]): Builds[Elem, To] = Builds wrap cbf
}
