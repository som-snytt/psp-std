package psp
package std

import api._
import scala.{ collection => sc }
import scala.Any
import scala.Predef.StringCanBuildFrom
import scala.math.Numeric

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

final class WalkableTypeClass[Repr](val repr: Repr) {
  def m[CC[X] <: Walkable[X]](implicit tc: CC[Repr]): AtomicView[tc.A, Repr] = tc wrap repr
}

// Adapt CanBuildFrom to Builds, since there are zillions of implicit CanBuildFroms already lying around.
// This lets us use all our own methods yet still build the scala type at the end, e.g.
//   Vector("a", "b", "cd", "ef").m filter (_.length == 1) build
// Returns a Vector[String].
trait StdBuilds0 extends Any                 { implicit def implicitBuildsFromCBF[A, That](implicit z: CanBuild[A, That]): Builds[A, That] = Builds wrap z          }
trait StdBuilds1 extends Any with StdBuilds0 { implicit def implicitBuildsArray[A: CTag] : Builds[A, Array[A]]                             = Direct.arrayBuilder[A] }
trait StdBuilds2 extends Any with StdBuilds1 { implicit def implicitBuildsList[A] : Builds[A, pSeq[A]]                                     = PolicyList.builder[A]  }
trait StdBuilds3 extends Any with StdBuilds2 { implicit def implicitBuildsSet[A: HashEq] : Builds[A, exSet[A]]                             = PolicySet.builder[A]   }
trait StdBuilds4 extends Any with StdBuilds3 { implicit def implicitBuildsDirect[A] : Builds[A, pVector[A]]                                = Direct.builder[A]      }
trait StdBuilds  extends Any with StdBuilds4 { implicit def implicitBuildsString: Builds[Char, String]                                     = Direct.stringBuilder() }

trait StdWalks0 extends Any {
  implicit def javaIterableIs[A] : ForeachableType[A, jIterable[A], jIterable]              = new Foreachable.JavaIterableIs[A]
  implicit def atomicForeachIs[A] : ForeachableType[A, Foreach[A], Foreach]                 = new Foreachable.ForeachIs[A]
  implicit def atomicTraversableIs[A] : ForeachableType[A, scTraversable[A], scTraversable] = new Foreachable.TraversableIs[A]
}
trait StdWalks1 extends Any with StdWalks0 {
  implicit def directScalaIndexedIs[A] : DirectAccessType[A, scIndexedSeq[A], scIndexedSeq] = new DirectAccess.ScalaIndexedIs[A]
}
trait StdWalks extends Any with StdWalks1 {
  implicit def extensionalSetIs[A] : ForeachableType[A, exSet[A], exSet]   = new Foreachable.ExtensionalSetIs[A]
  implicit def directIndexedIs[A] : DirectAccessType[A, Direct[A], Direct] = new DirectAccess.IndexedIs[A]
  implicit def directArrayIs[A] : DirectAccessType[A, Array[A], Direct]    = new DirectAccess.ArrayIs[A]
  implicit def directStringIs: DirectAccessType[Char, String, Direct]      = DirectAccess.StringIs
}

trait StdOps0 extends Any {
  implicit def numericToSums[A](implicit z: Numeric[A]): Sums[A]         = new Sums[A] { def zero = z.zero ; def sum(x: A, y: A): A = z.plus(x, y) }
  implicit def numericToProducts[A](implicit z: Numeric[A]): Products[A] = new Products[A] { def one = z.one ; def product(x: A, y: A): A = z.times(x, y) }

  implicit class ShowableToDocShown[A: Show](x: A) {
    def doc: Doc = Doc.Shown(x, ?[Show[A]])
  }
  implicit def opsForeach[A](xs: Foreach[A]): ops.ForeachOps[A]                     = new ops.ForeachOps(xs)
  implicit def opsJavaIterable[A](x: jIterable[A]): ops.jIterableOps[A]             = new ops.jIterableOps[A](x)
  implicit def opsIntensionalSet[A](x: IntensionalSet[A]): ops.IntensionalSetOps[A] = new ops.IntensionalSetOps[A](x)
}
trait StdOps1 extends Any with StdOps0 {
  implicit def opsScalaCollection[A, CC[X] <: sCollection[X]](x: CC[A]): ops.sCollectionOps[A, CC] = new ops.sCollectionOps[A, CC](x)
  implicit def opsDirect[A](xs: Direct[A]): ops.DirectOps[A]                                       = new ops.DirectOps(xs)
  implicit def arraySpecificOps[A](xs: Array[A]): ops.ArraySpecificOps[A]                          = new ops.ArraySpecificOps[A](xs)
  implicit def opsExtensionalSet[A](x: ExtensionalSet[A]): ops.ExtensionalSetOps[A]                = new ops.ExtensionalSetOps[A](x)
}
trait StdOps2 extends Any with StdOps1 {
  implicit def opsDirectArray[A](xs: Array[A]): ops.DirectOps[A] = new ops.DirectOps(Direct fromArray xs)
  implicit def opsDirectString(s: String): ops.DirectOps[Char]   = new ops.DirectOps(Direct fromString s)

  // We buried Predef's {un,}augmentString in favor of these.
  @inline final implicit def pspAugmentString(x: String): PspStringOps   = new PspStringOps(x)
}

trait StdOps3 extends Any with StdOps2 {
  implicit def infixOpsPartialOrder[A: PartialOrder](x: A): infix.PartialOrderOps[A] = new infix.PartialOrderOps[A](x)
  implicit def infixOpsOrder[A: Order](x: A): infix.OrderOps[A]                      = new infix.OrderOps[A](x)
  implicit def infixOpsAlgebra[A: BooleanAlgebra](x: A): infix.AlgebraOps[A]         = new infix.AlgebraOps[A](x)
  implicit def infixOpsEq[A: Eq](x: A): infix.EqOps[A]                               = new infix.EqOps[A](x)
  implicit def infixOpsHash[A: Hash](x: A): infix.HashOps[A]                         = new infix.HashOps[A](x)

  implicit def opsApiView[A](x: View[A]): ops.WeakApiViewOps[A]                       = new ops.WeakApiViewOps(x)
  implicit def opsBaseView[A, Repr](x: BaseView[A, Repr]): ops.BaseViewOps[A, Repr]   = new ops.BaseViewOps(x)
  implicit def opsBiFunction[T, R](f: (T, T) => R): ops.BiFunctionOps[T, R]           = new ops.BiFunctionOps(f)
  implicit def opsBoolean(x: Boolean): ops.BooleanOps                                 = new ops.BooleanOps(x)
  implicit def opsBooleanAlgebra[A](x: BooleanAlgebra[A]): ops.BooleanAlgebraOps[A]   = new ops.BooleanAlgebraOps[A](x)
  implicit def opsChar(x: Char): ops.CharOps                                          = new ops.CharOps(x)
  implicit def opsClassLoader(x: jClassLoader): ops.ClassLoaderOps                    = new ops.ClassLoaderOps(x)
  implicit def opsDoc(x: Doc): ops.DocOps                                             = new ops.DocOps(x)
  implicit def opsDocSeq(x: DocSeq): ops.DocSeqOps                                    = new ops.DocSeqOps(x)
  implicit def opsFileTime(x: jFileTime): ops.FileTimeOps                             = new ops.FileTimeOps(x)
  implicit def opsPartialFunction[A, B](pf: A ?=> B): ops.PartialFunctionOps[A, B]    = new ops.PartialFunctionOps(pf)
  implicit def opsFunction1[T, R](f: T => R): ops.Function1Ops[T, R]                  = new ops.Function1Ops(f)
  implicit def opsFunction2[T1, T2, R](f: (T1, T2) => R): ops.Function2Ops[T1, T2, R] = new ops.Function2Ops(f)
  implicit def opsGenerator[A](x: Generator[A]): ops.GeneratorOps[A]                  = new ops.GeneratorOps(x)
  implicit def opsHasPreciseSize(x: HasPreciseSize): ops.HasPreciseSizeOps            = new ops.HasPreciseSizeOps(x)
  implicit def opsIndexRange(x: IndexRange): ops.IndexRangeOps                        = new ops.IndexRangeOps(x)
  implicit def opsInputStream(x: InputStream): ops.InputStreamOps                     = new ops.InputStreamOps(x)
  implicit def opsInt(x: Int): ops.IntOps                                             = new ops.IntOps(x)
  implicit def opsLong(x: Long): ops.LongOps                                          = new ops.LongOps(x)
  implicit def opsOption[A](x: Option[A]): ops.OptionOps[A]                           = new ops.OptionOps[A](x)
  implicit def opsPrecise(x: Precise): ops.PreciseOps                                 = new ops.PreciseOps(x)
  implicit def opsPredicate[A](p: Predicate[A]): ops.PredicateOps[A]                  = new ops.PredicateOps(p)
  implicit def opsShowableSeq[A: Show](x: pSeq[A]): ops.ShowableSeqOps[A]             = new ops.ShowableSeqOps(x)
  implicit def opsSize(x: Size): Size.Ops                                             = new Size.Ops(x)
  implicit def opsStdOpt[A](x: Opt[A]): ops.StdOptOps[A]                              = new ops.StdOptOps[A](x)
  implicit def opsTry[A](x: Try[A]): ops.TryOps[A]                                    = new ops.TryOps[A](x)
  implicit def opsUnit(x: Unit): ops.UnitOps.type                                     = ops.UnitOps
}
trait StdOps extends Any with StdOps3 {
  implicit def opsApiShowInterpolator(sc: StringContext): ShowInterpolator              = new ShowInterpolator(sc)
  implicit def predicateToDirectoryFilter[A](p: Predicate[A]): DirectoryStreamFilter[A] = new DirectoryStreamFilter[A] { def accept(entry: A) = p(entry) }

  // Promotion of the api type (which has as few methods as possible) to the
  // concrete type which has all the other ones.
  implicit def apiIndexPromote(x: Index): IndexImpl                              = Index impl x
  implicit def apiIndexRangePromote(x: IndexRange): IntIndexRange                = IndexRange impl x
  implicit def apiOrderPromote[A](ord: Order[A]): Order.Impl[A]                  = Order(ord.compare)
  implicit def directoryStreamToIterable[A](stream: DirectoryStream[A]): pSeq[A] = BiIterable(stream).pseq
}

// Prefer opsAnyRef.
trait StdUniversal0 extends Any                   { implicit def opsAny[A](x: A): ops.AnyOps[A]                 = new ops.AnyOps[A](x)    }
trait StdUniversal extends Any with StdUniversal0 { implicit def opsAnyRef[A <: AnyRef](x: A): ops.AnyRefOps[A] = new ops.AnyRefOps[A](x) }

// This doesn't work if the return type is declared as tc.VC[Repr], or if it is inferred.
// def m[CC[X] <: Walkable[X]](implicit tc: CC[Repr]): tc.VC[Repr] = tc wrap repr
//
// [error] /mirror/r/psp/std/testOnly/src/test/scala/OperationCounts.scala:56: polymorphic expression cannot be instantiated to expected type;
// [error]  found   : [CC[X] <: psp.std.Walkable[X]]tc.VC[psp.std.PolicyList[psp.std.Int]]
// [error]  required: psp.std.api.View[psp.std.Int]
// [error]     intRange(1, max / 2).m ++ nthRange(max / 2, max).toLinear.m
// [error]                                                               ^
// [error] one error found
