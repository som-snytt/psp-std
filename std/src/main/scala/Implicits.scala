package psp
package std

import scala.{ collection => sc }

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
trait CollectionPackageLevel extends CollectionHigh {
  // With type aliases like these which include a type selection,
  // sometimes substitution fails and you get messages like
  // "found: Int, required: tc.A." It is bug, bug, bug city.
  // It can be worked a little bit by expanding the type
  // manually at the call sites where the bug hits (it's SI-8223).
  type AtomicView[Repr, W <: WalkableTypes]  = Env[Repr, W]#AtomicView
  type IndexedView[Repr, W <: WalkableTypes] = Env[Repr, W]#IndexedView
  type LinearView[Repr, W <: WalkableTypes]  = Env[Repr, W]#LinearView

  type Env[Repr, W <: WalkableTypes] = ViewEnvironment[W#A, Repr, W#CC]

  type ForeachableType[A0, Repr, CC0[X]] = Foreachable[Repr] {
    type A = A0
    type CC[B] = CC0[B]
  }
  type SequentialAccessType[A0, Repr, CC0[X]] = SequentialAccess[Repr] {
    type A = A0
    type CC[B] = CC0[B]
  }
  type DirectAccessType[A0, Repr, CC0[X]] = DirectAccess[Repr] {
    type A = A0
    type CC[B] = CC0[B]
  }
}

trait LowPriorityPspStd {
  // A weaker variation of Shown - use Show[A] if one can be found and toString otherwise.
  implicit def showableToTryShown[A](x: A)(implicit shows: Show[A] = Show.native[A]): TryShown = new TryShown(shows show x)
  // Deprioritize PartialOrder vs. Order since they both have comparison methods.
  implicit def tclassPartialOrderOps[A: PartialOrder](x: A): TClass.PartialOrderOps[A] = new TClass.PartialOrderOps[A](x)
  // Temporary compat with scala's Ordering.
  implicit def orderOrdering[A](implicit ord: Order[A]): Ordering[A] = Ordering fromLessThan ((x, y) => ord.compare(x, y).intValue < 0)
}

trait Implicits extends LowPriorityPspStd {
  // The typesafe non-toString-using show"..." interpolator.
  implicit def showStringContextOps(sc: StringContext): ShowInterpolator = new ShowInterpolator(sc)
  // Continuing the delicate dance against scala's hostile-to-correctness intrinsics.
  implicit def showableToShown[A: Show](x: A): Shown = new Shown(show[A] show x)

  // We buried Predef's {un,}augmentString in favor of these.
  @inline final implicit def pspAugmentString(x: String): PspStringOps   = new PspStringOps(x)
  @inline final implicit def pspUnaugmentString(x: PspStringOps): String = x.toString

  // Extension methods for non-collection types.

  // Extensions for psp-view.DoesForeach
  implicit def implicitForeachOps[A](xs: Foreach[A]): ForeachOperations[A] = new ForeachOperations(xs)
  implicit def implicitHasForeach[R: Has.Foreach](xs: R)                   = new ForeachOperations(Foreach(?[Has.Foreach[R]] hasForeach xs))

  implicit def directBuilder[A] : Builds[A, Direct[A]] = Builds((xs: Foreach[A]) =>
    xs match {
      case xs: Direct[A] => xs
      case _             => Direct.elems(xs.toSeq: _*)
    }
  )

  // Typeclass requiring extension methods. There is something very depraved going on here,
  // see if you can tell what it is.
  implicit def tclassShowDirectOps[A: Show](x: A): TClass.ShowDirectOps        = new TClass.ShowDirectOps(x.to_s)
  implicit def tclassOrderOps[A: Order](x: A): TClass.OrderOps[A]              = new TClass.OrderOps[A](x)
  implicit def tclassAlgebraOps[A: BooleanAlgebra](x: A): TClass.AlgebraOps[A] = new TClass.AlgebraOps[A](x)

  // Direct-acting extension methods. These are extension methods installed directly onto the
  // type of interest, as opposed to involving a typeclass.
  implicit def opsAny[A](x: A): Ops.AnyOps[A]                                         = new Ops.AnyOps[A](x)
  implicit def opsArray[A](xs: Array[A]): Ops.ArrayOps[A]                             = new Ops.ArrayOps[A](xs)
  implicit def opsBooleanAlgebra[A](alg: BooleanAlgebra[A]): Ops.BooleanAlgebraOps[A] = new Ops.BooleanAlgebraOps[A](alg)
  implicit def opsEq[A](x: Eq[A]): Ops.EqOps[A]                                       = new Ops.EqOps[A](x)
  implicit def opsFunction1[T, R](f: T => R): Ops.Function1Ops[T, R]                  = new Ops.Function1Ops[T, R](f)
  implicit def opsGTOnce[CC[X] <: GTOnce[X], A](xs: CC[A]): Ops.GTOnce[CC, A]         = new Ops.GTOnce[CC, A](xs)
  implicit def opsInputStream(x: InputStream): io.Ops.InputStreamOps                  = new io.Ops.InputStreamOps(x)
  implicit def opsInt(x: Int): Ops.IntOps                                             = new Ops.IntOps(x)
  implicit def opsLong(x: Long): Ops.LongOps                                          = new Ops.LongOps(x)
  implicit def opsMap[K, V](xs: sc.Map[K, V]): Ops.Map[K, V]                          = new Ops.Map[K, V](xs)
  implicit def opsOption[A](x: Option[A]): Ops.OptionOps[A]                           = new Ops.OptionOps[A](x)
  implicit def opsOrder[A](x: Order[A]): Ops.OrderOps[A]                              = new Ops.OrderOps[A](x)
  implicit def opsSeq1[CC[X] <: sc.Seq[X], A](xs: CC[A]): Ops.Seq1[CC, A]             = new Ops.Seq1[CC, A](xs)
  implicit def opsSeq2[CC[X] <: sc.Seq[X], A](xs: CC[A]): Ops.Seq2[CC, A]             = new Ops.Seq2[CC, A](xs)
  implicit def opsSeq[CC[X] <: sc.Seq[X], A](xs: CC[A]): Ops.Seq[CC, A]               = new Ops.Seq[CC, A](xs)
  implicit def opsShow[A](x: Show[A]): Ops.ShowOps[A]                                 = new Ops.ShowOps[A](x)
  implicit def opsSortedMap[K, V](xs: sc.SortedMap[K, V]): Ops.SortedMap[K, V]        = new Ops.SortedMap[K, V](xs)
  implicit def opsTry[A](x: scala.util.Try[A]): Ops.TryOps[A]                         = new Ops.TryOps[A](x)
}

/** Various lame global-scope implicits, made to disappear with our friend null.
 *  This list is subject to renegotiation.
 */
trait ImplicitRemoval {
  val any2stringadd              = null
  val fallbackStringCanBuildFrom = null
  val tuple2ToZippedOps          = null
  val tuple3ToZippedOps          = null
  val unwrapString               = null

  val Boolean2boolean = null
  val Byte2byte       = null
  val Character2char  = null
  val Double2double   = null
  val Float2float     = null
  val Integer2int     = null
  val Long2long       = null
  val Short2short     = null

  val genericWrapArray = null
  val wrapBooleanArray = null
  val wrapByteArray    = null
  val wrapCharArray    = null
  val wrapDoubleArray  = null
  val wrapFloatArray   = null
  val wrapIntArray     = null
  val wrapLongArray    = null
  val wrapRefArray     = null
  val wrapShortArray   = null
  val wrapString       = null
  val wrapUnitArray    = null

  // We reimplement these.
  val augmentString   = null
  val unaugmentString = null

  val byteArrayOps, shortArrayOps, charArrayOps, intArrayOps, longArrayOps, floatArrayOps, doubleArrayOps = null
  val byteWrapper, shortWrapper, charWrapper, intWrapper, longWrapper, floatWrapper, doubleWrapper        = null
  val StringAdd, ArrowAssoc                                                                               = null
  // val genericArrayOps                                                                                     = null
}
