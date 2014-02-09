package psp
package core

import impl._
import java.nio.file.Paths

trait PspUtilityMethods extends PspUniversals {
  def jClassOf[T: ClassTag] : jClass[T] = classTag[T].runtimeClass.castTo[jClass[T]]
  def classTag[T: ClassTag] : ClassTag[T] = implicitly[ClassTag[T]]

  def labelpf[T, R](label: String)(pf: T =?> R): T =?> R = new LabeledPartialFunction(pf, label)
  def failEmpty(operation: String): Nothing = throw new NoSuchElementException(s"$operation on empty collection")
  def fail(msg: String): Nothing            = throw new RuntimeException(msg)
}

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

trait ArrowAssocLow {
  @inline final implicit def arrowAssocRef[A](x: A): ArrowAssocRef[A]    = new ArrowAssocRef(x)
}
trait ArrowAssocHigh extends ArrowAssocLow {
 @inline final implicit def arrowAssocInt(x: Int): ArrowAssocInt             = new ArrowAssocInt(x)
 @inline final implicit def arrowAssocLong(x: Long): ArrowAssocLong          = new ArrowAssocLong(x)
 @inline final implicit def arrowAssocDouble(x: Double): ArrowAssocDouble    = new ArrowAssocDouble(x)
 @inline final implicit def arrowAssocChar(x: Char): ArrowAssocChar          = new ArrowAssocChar(x)
 @inline final implicit def arrowAssocBoolean(x: Boolean): ArrowAssocBoolean = new ArrowAssocBoolean(x)
}

trait PspUniversals extends ArrowAssocHigh {
  @inline final implicit def raisePspInt(x: Int): PspInt                 = new PspInt(x)
  @inline final implicit def raiseUniversalOps[T](x: T): UniversalOps[T] = new UniversalOps(x)
}

trait PspLowPriority extends PspUniversals {
  // See lowerNativeView.
  implicit def lowerGenericView[A, B, CC[X]](xs: ViewEnvironment[A, _, CC]#View[B])(implicit pcb: Builds[B, CC[B]]): CC[B] = xs.force
}

trait PspMidPriority extends PspLowPriority {
  implicit def raisePartiallyOrderOps[A](x: PartiallyOrdered[A]): PartiallyOrderedOps[A]                           = new PartiallyOrderedOps(x)
  implicit def raisePspStringOps(s: String): PspStringOps                                                          = new PspStringOps(s)
  implicit def lowerPspStringOps(s: PspStringOps): String                                                          = s.repr

  // I don't think this should be implicit, but people are so easily impressed
  // and so easily latch onto irrelevant details, we are sort of forced to be
  // gimmick-compatible with scala to silence them.
  implicit def lowerNativeView[A, Repr, CC[X]](xs: ViewEnvironment[A, Repr, CC]#View[A])(implicit pcb: Builds[A, Repr]): Repr = xs.native
}

trait PspHighPriority extends PspMidPriority with CollectionHigh {
  implicit def raisePartialFunctionOps[T, R](pf: T =?> R): PartialFunctionOps[T, R]                                                = new PartialFunctionOps[T, R](pf)
  implicit def raiseFunction1Ops[T, R](f: T => R): Function1Ops[T, R]                                                              = new Function1Ops[T, R](f)
  implicit def raiseFunction2Ops[T1, T2, R](f: (T1, T2) => R): Function2Ops[T1, T2, R]                                             = new Function2Ops(f)
  implicit def raiseExtraViewOps[A, B, Repr, CC[X]](xs: ViewEnvironment[A, Repr, CC]#View[B]): ExtraViewOperations[A, B, Repr, CC] = new ExtraViewOperations[A, B, Repr, CC](xs)
  implicit def raisePpInterpolatorOps(sc: StringContext): PpInterpolatorOps                                                        = new PpInterpolatorOps(sc)
  implicit def raiseJavaPathOps(p: jPath): JavaPathOps                                                                             = new JavaPathOps(p)

  implicit def raiseForeachableBuilderOps[Repr](tc: Foreachable[Repr]): ForeachableBuilderOps[tc.A, Repr, tc.CC]    = new ForeachableBuilderOps[tc.A, Repr, tc.CC](tc)
  implicit def raiseDirectAccessBuilderOps[Repr](tc: DirectAccess[Repr]): DirectAccessBuilderOps[tc.A, Repr, tc.CC] = new DirectAccessBuilderOps[tc.A, Repr, tc.CC](tc)

  implicit def convertCanBuildFrom[Elem, To](implicit cbf: CanBuildFrom[_, Elem, To]): Builds[Elem, To] = Builds wrap cbf
}

class ForeachableBuilderOps[A, Repr, CC[X]](tc: ForeachableType[A, Repr, CC]) {
  def genericBuilder[B]: Builder[B, CC[B]] = ??? // pcb.newBuilder()
  def nativeBuilder: Builder[A, Repr]      = ??? // pcb.newBuilder()
}

class DirectAccessBuilderOps[A, Repr, CC[X]](tc: DirectAccessType[A, Repr, CC]) {
  def genericBuilder[B]: Builder[B, CC[B]] = ??? // pcb.newBuilder()
  def nativeBuilder: Builder[A, Repr]      = ??? // pcb.newBuilder()
}

trait PspShadowRequired {
  val StringAdd, ArrowAssoc = null
}

/** It's kind of funny... I guess.
 */
trait PspShadowScala extends PspShadowRequired {
  val wrapByteArray, wrapShortArray, wrapCharArray, wrapIntArray, wrapLongArray, wrapFloatArray, wrapDoubleArray = null
  val byteArrayOps, shortArrayOps, charArrayOps, intArrayOps, longArrayOps, floatArrayOps, doubleArrayOps        = null
  val byteWrapper, shortWrapper, charWrapper, intWrapper, longWrapper, floatWrapper, doubleWrapper               = null
  val wrapString, unwrapString, augmentString, unaugmentString                                                   = null
  val genericArrayOps, genericWrapArray                                                                          = null
}
// ViewEnvironment[A, Repr, CC]#View[B]

trait JioCreation {
  def path(path: String): jPath = Paths get path
  def file(path: String): jFile = new jFile(path)
  def url(path: String): jUrl   = new jUrl(path)
}
