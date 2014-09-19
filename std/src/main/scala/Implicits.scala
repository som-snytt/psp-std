package psp
package std

import scala.{ collection => sc }
import api.Regex

/** Yes I know all about implicit classes.
 *  There's no way to write an implicit value class which doesn't hardcode
 *  its location into an object. Separating the implicit conversion from
 *  the class allows clients to build their own package object.
 *
 *  This is all a consequence of scala offering no means for managing namespaces,
 *  so namespace management has become hopelessly entangled with unrelated concerns
 *  like inheritance, specificity, method dispatch, and so forth.
 */
abstract class PackageImplicits extends ImplicitRemoval
      with StandardImplicits2
      with ArrowAssoc2
      with ShowImplicits
      with ReadImplicits
      with OrderImplicits
      with EqImplicits {
}

/** Various lame global-scope implicits, made to disappear with our friend null.
 *  This list is subject to renegotiation.
 */
trait ImplicitRemoval {
  val any2stringadd, fallbackStringCanBuildFrom                                                           = null
  val tuple2ToZippedOps, tuple3ToZippedOps                                                                = null
  val wrapString, unwrapString, augmentString, unaugmentString                                            = null
  val StringAdd, ArrowAssoc, Boolean2boolean, Byte2byte, Character2char                                   = null
  val Double2double, Float2float, Integer2int, Long2long, Short2short                                     = null
  val genericWrapArray, wrapBooleanArray, wrapByteArray, wrapCharArray, wrapDoubleArray, wrapFloatArray   = null
  val wrapIntArray, wrapLongArray, wrapRefArray, wrapShortArray, wrapUnitArray                            = null
  val byteWrapper, shortWrapper, charWrapper, intWrapper, longWrapper, floatWrapper, doubleWrapper        = null
  val byteArrayOps, shortArrayOps, charArrayOps, intArrayOps, longArrayOps, floatArrayOps, doubleArrayOps = null
  val genericArrayOps                                                                                     = null

  // 2.10
  val any2ArrowAssoc = null
}

trait StandardImplicits1 {
  implicit def sizeToSizeInfo(s: Size): SizeInfo    = s.toInfo
  implicit def apiSizeToSize(s: api.Size): Size     = Size(s.value)
  implicit def apiIndexToIndex(x: api.Index): Index = Index(x.value)

  implicit def implicitListBuilder[A] : Builds[A, PspList[A]]                 = Builds(_.foldr(PspList.empty[A])(_ :: _).reverse)
  implicit def implicitArrayBuilder[A: ClassTag] : Builds[A, Array[A]]        = Builds(Array.newBuilder[A] ++= _.trav result)
  implicit def implicitDirectBuilder[A] : Builds[A, Direct[A]]                = Builds(_.toDirect)
  implicit def walkableOps[Repr, A0](repr: Repr)(implicit tc: Walkable[Repr]) = new OpsContainer(() => tc wrap repr)

  // Deprioritize PartialOrder vs. Order since they both have comparison methods.
  implicit def tclassPartialOrderOps[A: PartialOrder](x: A): TClass.PartialOrderOps[A] = new TClass.PartialOrderOps[A](x)
}

trait StandardImplicits2 extends StandardImplicits1 {
  // We buried Predef's {un,}augmentString in favor of these.
  @inline final implicit def pspAugmentString(x: String): PspStringOps   = new PspStringOps(x)
  @inline final implicit def pspUnaugmentString(x: PspStringOps): String = x.toString

  // Typeclass requiring extension methods. There is something very depraved going on here,
  // see if you can tell what it is.
  implicit def tclassOrderOps[A: Order](x: A): TClass.OrderOps[A]              = new TClass.OrderOps[A](x)
  implicit def tclassAlgebraOps[A: BooleanAlgebra](x: A): TClass.AlgebraOps[A] = new TClass.AlgebraOps[A](x)
  implicit def tclassEqOps[A: Eq](x: A): TClass.EqOps[A]                       = new TClass.EqOps[A](x)
  implicit def tclassHashEqOps[A: HashEq](x: A): TClass.HashEqOps[A]           = new TClass.HashEqOps[A](x)
  implicit def tclassHasForeach[R: Has.Foreach](xs: R)                         = opsForeach(Foreach(?[Has.Foreach[R]] hasForeach xs))

  // Direct-acting extension methods. These are extension methods installed directly onto the
  // type of interest, as opposed to involving a typeclass.
  implicit def opsAny[A](x: A): Ops.AnyOps[A]                                         = new Ops.AnyOps[A](x)
  implicit def opsArray[A](xs: Array[A]): Ops.ArrayOps[A]                             = new Ops.ArrayOps[A](xs)
  implicit def opsBooleanAlgebra[A](alg: BooleanAlgebra[A]): Ops.BooleanAlgebraOps[A] = new Ops.BooleanAlgebraOps[A](alg)
  implicit def opsCollection[A](x: jAbstractCollection[A]): Ops.jCollectionOps[A]     = new Ops.jCollectionOps(x)
  implicit def opsForeach[A](xs: Foreach[A]): Ops.ForeachOps[A]                       = new Ops.ForeachOps(xs)
  implicit def opsFunction1[T, R](f: T => R): Ops.Function1Ops[T, R]                  = new Ops.Function1Ops[T, R](f)
  implicit def opsGTOnce[A](xs: GTOnce[A]): Ops.GTOnceOps[A]                          = new Ops.GTOnceOps[A](xs)
  implicit def opsInputStream(x: InputStream): Ops.InputStreamOps                     = new Ops.InputStreamOps(x)
  implicit def opsInt(x: Int): Ops.IntOps                                             = new Ops.IntOps(x)
  implicit def opsIterator[A](it: jIterator[A]): Ops.IteratorOps[A]                   = new Ops.IteratorOps(it)
  implicit def opsLong(x: Long): Ops.LongOps                                          = new Ops.LongOps(x)
  implicit def opsMap[K, V](xs: sc.Map[K, V]): Ops.Map[K, V]                          = new Ops.Map[K, V](xs)
  implicit def opsSeq1[A](xs: sc.Seq[A]): Ops.Seq1[A]                                 = new Ops.Seq1[A](xs)
  implicit def opsSeq2[A](xs: sc.Seq[A]): Ops.Seq2[A]                                 = new Ops.Seq2[A](xs)
  implicit def opsSeqOps[A](xs: sc.Seq[A]): Ops.SeqOps[A]                             = new Ops.SeqOps[A](xs)
  implicit def opsSizeInfo(x: SizeInfo): Ops.SizeInfoOps                              = new Ops.SizeInfoOps(x)
  implicit def opsSortedMap[K, V](xs: sc.SortedMap[K, V]): Ops.SortedMap[K, V]        = new Ops.SortedMap[K, V](xs)
}

trait ReadImplicits {
  implicit val bigDecRead: Read[BigDecimal] = Read(s => BigDecimal(s))
  implicit val bigIntRead: Read[BigInt]     = Read(s => BigInt(s))
  implicit val doubleRead: Read[Double]     = Read(_.toDouble)
  implicit val floatRead: Read[Float]       = Read(_.toFloat)
  implicit val intRead: Read[Int]           = Read(_.toInt)
  implicit val longRead: Read[Long]         = Read(_.toLong)
  implicit val stringRead: Read[String]     = Read(s => s)
  implicit val uriRead: Read[URI]           = Read(uri)
  implicit val regexRead: Read[Regex]       = Read(Regex)
}

trait OrderImplicits {
  implicit val booleanOrder: Order[Boolean] = Order[Boolean](_ compare _ cmp)
  implicit val byteOrder: Order[Byte]       = Order[Byte](_ - _ cmp)
  implicit val charOrder: Order[Char]       = Order[Char](_ - _ cmp)
  implicit val intOrder: Order[Int]         = Order[Int](_ - _ cmp)
  implicit val longOrder: Order[Long]       = Order[Long](_ - _ cmp)
  implicit val shortOrder: Order[Short]     = Order[Short](_ - _ cmp)
  implicit val stringOrder: Order[String]   = Order[String](_ compareTo _ cmp)

  implicit def indexOrder: Order[api.Index]                                 = orderBy[api.Index](_.value)
  implicit def sizeOrder: Order[api.Size]                                   = orderBy[api.Size](_.value)
  implicit def tuple2Order[A: Order, B: Order] : Order[(A, B)]              = Order[(A, B)]((x, y) => Order.fold(x._1 compare y._1, x._2 compare y._2))
  implicit def tuple3Order[A: Order, B: Order, C: Order] : Order[(A, B, C)] = Order[(A, B, C)]((x, y) => Order.fold(x._1 compare y._1, x._2 compare y._2, x._3 compare y._3))

  // no, infinity doesn't really equal infinity, but it can for our
  // purposes as long as <inf> - <inf> is ill-defined.
  implicit object sizeInfoPartialOrder extends PartialOrder[SizeInfo] {
    import psp.std.api.PCmp
    import SizeInfo.GenBounded

    def partialCompare(lhs: SizeInfo, rhs: SizeInfo): PCmp = (lhs, rhs) match {
      case (Infinite, Infinite)                     => PCmp.EQ
      case (Precise(_), Infinite)                   => PCmp.LT
      case (Infinite, Precise(_))                   => PCmp.GT
      case (Precise(x), Precise(y))                 => if (x < y) PCmp.LT else if (y < x) PCmp.GT else PCmp.EQ
      case (Infinite, Bounded(_, Infinite))         => PCmp.NA
      case (Infinite, _)                            => PCmp.GT
      case (Bounded(_, Infinite), Infinite)         => PCmp.NA
      case (_, Infinite)                            => PCmp.LT
      case (GenBounded(l1, h1), GenBounded(l2, h2)) =>
        def lo1 = Precise(l1)
        def lo2 = Precise(l2)
        if (h1 p_< lo2) PCmp.LT else if (h2 p_< lo1) PCmp.GT else PCmp.NA
    }
  }
}

trait ShowImplicits extends api.ShowImplicits {
  implicit def viewShow[A] : Show[api.View[A]]         = Show(_.viewChain reverseMap (_.description) joinSpace)
  implicit def pspListShow[A: Show] : Show[PspList[A]] = Show(xs => if (xs.isEmpty) "nil" else (xs join " :: ") + " :: nil")
}

trait EqImplicits {
  def setEq[CC[X] <: Set[X], A: HashEq] : Eq[CC[A]] = Eq[CC[A]]((xs, ys) => each(xs).toSet === each(ys).toSet)

  implicit def sizeInfoEq: Eq[SizeInfo]            = Eq(_ == _)
  implicit def mapEq[K: Eq, V: Eq] : Eq[Map[K, V]] = Eq((xs, ys) => each(xs.keys).toSet === each(ys.keys).toSet && xs.keys.forall(k => xs(k) === ys(k)))
  implicit def seqEq[A: Eq] : Eq[Seq[A]]           = Eq((xs, ys) => (xs corresponds ys)(_ === _))
  implicit def arrayEq[A: Eq] : Eq[Array[A]]       = Eq(_.toSeq == _.toSeq)
}

trait ArrowAssoc1 {
  @inline final implicit def arrowAssocRef[A](x: A): Ops.ArrowAssocRef[A] = new Ops.ArrowAssocRef(x)
}
trait ArrowAssoc2 extends ArrowAssoc1 {
  @inline final implicit def arrowAssocInt(x: Int): Ops.ArrowAssocInt             = new Ops.ArrowAssocInt(x)
  @inline final implicit def arrowAssocLong(x: Long): Ops.ArrowAssocLong          = new Ops.ArrowAssocLong(x)
  @inline final implicit def arrowAssocDouble(x: Double): Ops.ArrowAssocDouble    = new Ops.ArrowAssocDouble(x)
  @inline final implicit def arrowAssocChar(x: Char): Ops.ArrowAssocChar          = new Ops.ArrowAssocChar(x)
  @inline final implicit def arrowAssocBoolean(x: Boolean): Ops.ArrowAssocBoolean = new Ops.ArrowAssocBoolean(x)
}
