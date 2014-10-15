package psp
package std

import api._
import scala.{ collection => sc }
import scala.Any
import scala.Predef.StringCanBuildFrom

/** Implicits handling the way in and the way out of policy collections.
 */
trait StdGateways extends Any
      with StdBuilds
      with StdWalks
      with StdOps
      with StdUniversal {

  self =>

  // The key implicit.
  implicit def walkableTypeClass[Repr](repr: Repr): WalkableTypeClass[Repr] = new WalkableTypeClass[Repr](repr)
}

// Adapt CanBuildFrom to Builds, since there are zillions of implicit CanBuildFroms already lying around.
// This lets us use all our own methods yet still build the scala type at the end, e.g.
//   Vector("a", "b", "cd", "ef").m filter (_.length == 1) build
// Returns a Vector[String].

trait StdBuilds0 extends Any                 { implicit def implicitBuildsFromCBF[A, That](implicit z: CanBuild[A, That]): Builds[A, That] = Builds wrap z          }
trait StdBuilds1 extends Any with StdBuilds0 { implicit def implicitBuildsList[A] : Builds[A, PolicyList[A]]                               = PolicyList.builder[A]  }
trait StdBuilds2 extends Any with StdBuilds1 { implicit def implicitBuildsDirect[A] : Builds[A, Direct[A]]                                 = Direct.builder[A]      }
trait StdBuilds3 extends Any with StdBuilds2 { implicit def implicitBuildsArray[A: CTag] : Builds[A, Array[A]]                             = Direct.arrayBuilder[A] }
trait StdBuilds  extends Any with StdBuilds3 { implicit def implicitBuildsString: Builds[Char, String]                                     = Direct.stringBuilder() }

trait StdShow0 {
  // A weaker variation of Shown - use Show[A] if one can be found and toString otherwise.
  implicit def showableToTryShown[A](x: A)(implicit z: TryShow[A]): TryShown = new TryShown(z show x)
}
trait StdShow1 extends StdShow0 {
  implicit def showableToShown[A](x: A)(implicit z: Show[A]): Shown = Shown(z show x)
}
trait StdShowLow extends StdShow1 {
  implicit def stringShow: Show[String] = Show(x => x)
  implicit def charShow: Show[Char]     = Show.natural()

  implicit class seqShowOps[A: Show](xs: Foreach[A]) {
    private def mkstr(sep: String): String = xs map (_.to_s) mkString sep
    def mk_s(sep: String): Shown           = Shown(mkstr(sep))

    def joinWith: Shown     = mk_s(" with ")
    def joinComma: Shown    = mk_s(", ")
    def joinInParens: Shown = Shown("(", joinComma, ")")
    def optBrackets: Shown  = Shown(mkstr(", ") mapNonEmpty ("[" + _ + "]"))
  }
}

trait StdWalks0 extends Any {
  implicit def atomicForeachIs[A] : ForeachableType[A, Foreach[A], Foreach]                 = new Foreachable.ForeachIs[A]
  implicit def atomicTraversableIs[A] : ForeachableType[A, scTraversable[A], scTraversable] = new Foreachable.TraversableIs[A]
}
trait StdWalks1 extends Any with StdWalks0 {
  implicit def directScalaIndexedIs[A] : DirectAccessType[A, IndexedSeq[A], IndexedSeq] = new DirectAccess.ScalaIndexedIs[A]
}
trait StdWalks extends Any with StdWalks1 {
  implicit def policySetIs[A] : ForeachableType[A, PolicySet[A], PolicySet] = new Foreachable.PolicySetIs[A]
  implicit def directIndexedIs[A] : DirectAccessType[A, Direct[A], Direct]  = new DirectAccess.IndexedIs[A]
  implicit def directArrayIs[A] : DirectAccessType[A, Array[A], Direct]     = new DirectAccess.ArrayIs[A]
  implicit def directStringIs: DirectAccessType[Char, String, Direct]       = DirectAccess.StringIs
}

trait StdOps0 extends Any {
  implicit def opsForeach[A](xs: Foreach[A]): ops.ForeachOps[A]         = new ops.ForeachOps(xs)
  implicit def opsJavaIterable[A](x: jIterable[A]): ops.jIterableOps[A] = new ops.jIterableOps[A](x)
}
trait StdOps1 extends Any with StdOps0 {
  implicit def opsScalaCollection[A, CC[X] <: sCollection[X]](x: CC[A]): ops.sCollectionOps[A, CC] = new ops.sCollectionOps[A, CC](x)
  implicit def opsDirect[A](xs: Direct[A]): ops.DirectOps[A]                                       = new ops.DirectOps(xs)
  implicit def arraySpecificOps[A](xs: Array[A]): ops.ArraySpecificOps[A]                          = new ops.ArraySpecificOps[A](xs)
}
trait StdOps2 extends Any with StdOps1 {
  implicit def opsDirectArray[A](xs: Array[A]): ops.DirectOps[A] = new ops.DirectOps(Direct fromArray xs)
  implicit def opsDirectString(s: String): ops.DirectOps[Char]   = new ops.DirectOps(Direct fromString s)

  // We buried Predef's {un,}augmentString in favor of these.
  @inline final implicit def pspAugmentString(x: String): PspStringOps   = new PspStringOps(x)
  @inline final implicit def pspUnaugmentString(x: PspStringOps): String = x.toString
}

trait StdOps3 extends Any with StdOps2 {
  implicit def infixOpsPartialOrder[A: PartialOrder](x: A): infix.PartialOrderOps[A] = new infix.PartialOrderOps[A](x)
  implicit def infixOpsOrder[A: Order](x: A): infix.OrderOps[A]                      = new infix.OrderOps[A](x)
  implicit def infixOpsLabelable[A: Labelable](x: A): infix.LabelableOps[A]          = new infix.LabelableOps[A](x)
  implicit def infixOpsAlgebra[A: BooleanAlgebra](x: A): infix.AlgebraOps[A]         = new infix.AlgebraOps[A](x)
  implicit def infixOpsEq[A: Eq](x: A): infix.EqOps[A]                               = new infix.EqOps[A](x)
  implicit def infixOpsHash[A: Hash](x: A): infix.HashOps[A]                         = new infix.HashOps[A](x)

  implicit def opsChar(x: Char): ops.CharOps                                   = new ops.CharOps(x)
  implicit def opsClass(x: jClass): ops.ClassOps                               = new ops.ClassOps(x)
  implicit def opsClassLoader(x: jClassLoader): ops.ClassLoaderOps             = new ops.ClassLoaderOps(x)
  implicit def opsFileTime(x: jFileTime): ops.FileTimeOps                      = new ops.FileTimeOps(x)
  implicit def opsFunction1[T, R](f: T => R): ops.Function1Ops[T, R]           = new ops.Function1Ops[T, R](f)
  implicit def opsInputStream(x: InputStream): ops.InputStreamOps              = new ops.InputStreamOps(x)
  implicit def opsInt(x: Int): ops.IntOps                                      = new ops.IntOps(x)
  implicit def opsLong(x: Long): ops.LongOps                                   = new ops.LongOps(x)
  implicit def opsMap[K, V](xs: scMap[K, V]): ops.Map[K, V]                    = new ops.Map[K, V](xs)
  implicit def opsOption[A](x: Option[A]): ops.OptionOps[A]                    = new ops.OptionOps[A](x)
  implicit def opsPredicate[A](f: Predicate[A]): ops.PredicateOps[A]           = new ops.PredicateOps(f)
  implicit def opsSizeInfo(x: SizeInfo): SizeInfo.Ops                          = new SizeInfo.Ops(x)
  implicit def opsSortedMap[K, V](xs: sc.SortedMap[K, V]): ops.SortedMap[K, V] = new ops.SortedMap[K, V](xs)
  implicit def opsStdOpt[A](x: Opt[A]): ops.StdOptOps[A]                       = new ops.StdOptOps[A](x)
  implicit def opsTry[A](x: Try[A]): ops.TryOps[A]                             = new ops.TryOps[A](x)
}
trait StdOps extends Any with StdOps3 {
  implicit def opsApiShowInterpolator(sc: StringContext): ShowInterpolator              = new ShowInterpolator(sc)
  implicit def predicateToDirectoryFilter[A](p: Predicate[A]): DirectoryStreamFilter[A] = new DirectoryStreamFilter[A] { def accept(entry: A) = p(entry) }
  implicit def sizeToSizeInfo(s: Size): SizeInfo                                        = s.fold(SizeInfo.unknown)(SizeInfo.precise)

  // Promotion of the api type (which has as few methods as possible) to the
  // concrete type which has all the other ones.
  implicit def apiOffsetPromote(x: Offset): IntOffset             = Offset impl x
  implicit def apiSizePromote(x: Size): IntSize                   = Size impl x
  implicit def apiIndexLikePromote(x: IndexLike): IntIndex        = Index impl x.toIndex
  implicit def apiIndexRangePromote(x: IndexRange): IntIndexRange = IndexRange impl x
  implicit def apiOrderPromote[A](ord: Order[A]): Order.Impl[A]   = Order(ord.compare)
  implicit def directoryStreamToIterable[A](stream: DirectoryStream[A]): pSeq[A] = BiIterable(stream).pseq
}

// Prefer opsAnyRef.
trait StdUniversal0 extends Any                   { implicit def opsAny[A](x: A): ops.AnyOps[A]                 = new ops.AnyOps[A](x)    }
trait StdUniversal extends Any with StdUniversal0 { implicit def opsAnyRef[A <: AnyRef](x: A): ops.AnyRefOps[A] = new ops.AnyRefOps[A](x) }

final class WalkableTypeClass[Repr](val repr: Repr) {
  def m[CC[X] <: Walkable[X]](implicit tc: CC[Repr]): AtomicView[tc.A, Repr] = tc wrap repr
}

// This doesn't work if the return type is declared as tc.VC[Repr], or if it is inferred.
// def m[CC[X] <: Walkable[X]](implicit tc: CC[Repr]): tc.VC[Repr] = tc wrap repr
//
// [error] /mirror/r/psp/std/testOnly/src/test/scala/OperationCounts.scala:56: polymorphic expression cannot be instantiated to expected type;
// [error]  found   : [CC[X] <: psp.std.Walkable[X]]tc.VC[psp.std.PolicyList[psp.std.Int]]
// [error]  required: psp.std.api.View[psp.std.Int]
// [error]     intRange(1, max / 2).m ++ nthRange(max / 2, max).toLinear.m
// [error]                                                               ^
// [error] one error found
