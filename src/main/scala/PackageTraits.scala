package psp
package core

import impl._

trait PspLowPriority {
  @inline final implicit def raisePspInt(x: Int): PspInt                 = new PspInt(x)
  @inline final implicit def raiseUniversalOps[T](x: T): UniversalOps[T] = new UniversalOps(x)
  @inline final implicit def arrowAssocRef[A](x: A): ArrowAssocRef[A]    = new ArrowAssocRef(x)
  implicit def raiseAtomicView[Repr](repr: Repr)(implicit tc: Foreachable[Repr]): AtomicView[Repr, tc.CC, tc.A] = AtomicView.unknown(repr)

  // I don't think this should be implicit, but people are so easily impressed
  // and so easily latch onto irrelevant details, we are sort of forced to be
  // gimmick-compatible with scala to silence them.
  implicit def lowerNativeView[Repr, CC[X], A](xs: ViewEnvironment[Repr, CC, A]#View[A])(implicit pcb: PspCanBuild[A, Repr]): Repr = xs.native
}

trait PspMidPriority extends PspLowPriority {
  implicit def raiseLinearableView[Repr](repr: Repr)(implicit tc: Linearable[Repr]): LinearView[Repr, tc.CC, tc.A] = AtomicView.linear(repr)
  implicit def raisePartiallyOrderOps[A](x: PartiallyOrdered[A]): PartiallyOrderedOps[A]                           = new PartiallyOrderedOps(x)
  implicit def raisePspStringOps(s: String): PspStringOps                                                          = new PspStringOps(s)
  implicit def lowerPspStringOps(s: PspStringOps): String                                                          = s.repr

  @inline final implicit def arrowAssocInt(x: Int): ArrowAssocInt             = new ArrowAssocInt(x)
  @inline final implicit def arrowAssocLong(x: Long): ArrowAssocLong          = new ArrowAssocLong(x)
  @inline final implicit def arrowAssocDouble(x: Double): ArrowAssocDouble    = new ArrowAssocDouble(x)
  @inline final implicit def arrowAssocChar(x: Char): ArrowAssocChar          = new ArrowAssocChar(x)
  @inline final implicit def arrowAssocBoolean(x: Boolean): ArrowAssocBoolean = new ArrowAssocBoolean(x)
}

trait PspHighPriority extends PspMidPriority {
  implicit def raiseIndexedView[Repr](repr: Repr)(implicit tc: Indexable[Repr]): IndexedView[Repr, tc.CC, tc.A] = AtomicView.indexed(repr)
  implicit def raisePartialFunctionOps[T, R](pf: T =?> R): PartialFunctionOps[T, R]                             = new PartialFunctionOps[T, R](pf)
  implicit def raiseFunctionOps[T, R](f: T => R): Function1Ops[T, R]                                            = new Function1Ops[T, R](f)
  implicit def raisePpInterpolatorOps(sc: StringContext): PpInterpolatorOps                                     = new PpInterpolatorOps(sc)
}

/** It's kind of funny... I guess.
 */
trait PspShadowScala {
  val wrapByteArray, wrapShortArray, wrapCharArray, wrapIntArray, wrapLongArray, wrapFloatArray, wrapDoubleArray = null
  val byteArrayOps, shortArrayOps, charArrayOps, intArrayOps, longArrayOps, floatArrayOps, doubleArrayOps        = null
  val byteWrapper, shortWrapper, charWrapper, intWrapper, longWrapper, floatWrapper, doubleWrapper               = null
  val wrapString, unwrapString, augmentString, unaugmentString                                                   = null
  val StringAdd, ArrowAssoc                                                                                      = null
  val genericArrayOps, genericWrapArray                                                                          = null
}
