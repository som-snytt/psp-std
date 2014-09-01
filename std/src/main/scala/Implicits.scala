package psp
package std

import scala.{ collection => sc }

abstract class PackageImplicits extends ImplicitRemoval
    with StandardImplicits4
    with ArrowAssoc2
    with ShowImplicits
    with ReadImplicits
    with OrderImplicits
    with EqImplicits

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
}

trait StandardImplicits1 {
  implicit def sizeToSizeInfo(s: Size): SizeInfo    = s.toInfo
  implicit def apiSizeToSize(s: api.Size): Size     = Size(s.value)
  implicit def apiIndexToIndex(x: api.Index): Index = Index(x.value)

  implicit def implicitListBuilder[A] : Builds[A, PspList[A]]          = Builds(_.foldr(PspList.empty[A])(_ :: _).reverse)
  implicit def implicitDirectBuilder[A] : Builds[A, Direct[A]]         = Builds(_.toDirect)
  implicit def implicitArrayBuilder[A: ClassTag] : Builds[A, Array[A]] = Builds(Array.newBuilder[A] ++= _.trav result)

  // aka AtomicView[Repr, tc.type] but SI-8223. Similarly for the analogous implicits.
  implicit def raiseAtomicView[Repr](repr: Repr)(implicit tc: Foreachable[Repr]): Env[Repr, tc.type]#AtomicView = tc wrap repr
}
trait StandardImplicits2 extends StandardImplicits1 {
  implicit def raiseIndexedView[Repr](repr: Repr)(implicit tc: DirectAccess[Repr]): Env[Repr, tc.type]#IndexedView = tc wrap repr
}

trait StandardImplicits3 extends StandardImplicits2 {
  // A weaker variation of Shown - use Show[A] if one can be found and toString otherwise.
  implicit def showableToTryShown[A](x: A)(implicit shows: Show[A] = Show.native[A]): TryShown = new TryShown(shows show x)
  // Deprioritize PartialOrder vs. Order since they both have comparison methods.
  implicit def tclassPartialOrderOps[A: PartialOrder](x: A): TClass.PartialOrderOps[A] = new TClass.PartialOrderOps[A](x)
}

trait StandardImplicits4 extends StandardImplicits3 {
  // The typesafe non-toString-using show"..." interpolator.
  implicit def showStringContextOps(sc: StringContext): ShowInterpolator = new ShowInterpolator(sc)
  // Continuing the delicate dance against scala's hostile-to-correctness intrinsics.
  implicit def showableToShown[A: Show](x: A): Shown = new Shown(show[A] show x)
  implicit def showLabeled = Show[api.Labeled](_.label)

  // We buried Predef's {un,}augmentString in favor of these.
  @inline final implicit def pspAugmentString(x: String): PspStringOps   = new PspStringOps(x)
  @inline final implicit def pspUnaugmentString(x: PspStringOps): String = x.toString

  // Typeclass requiring extension methods. There is something very depraved going on here,
  // see if you can tell what it is.
  implicit def tclassShowDirectOps[A: Show](x: A): TClass.ShowDirectOps        = new TClass.ShowDirectOps(x.to_s)
  implicit def tclassOrderOps[A: Order](x: A): TClass.OrderOps[A]              = new TClass.OrderOps[A](x)
  implicit def tclassAlgebraOps[A: BooleanAlgebra](x: A): TClass.AlgebraOps[A] = new TClass.AlgebraOps[A](x)
  implicit def tclassEqOps[A: Eq](x: A): TClass.EqOps[A]                       = new TClass.EqOps[A](x)
  implicit def tclassEqOps[A: HashEq](x: A): TClass.HashEqOps[A]               = new TClass.HashEqOps[A](x)
  implicit def tclassHasForeach[R: Has.Foreach](xs: R)                         = opsForeach(Foreach(?[Has.Foreach[R]] hasForeach xs))

  // Direct-acting extension methods. These are extension methods installed directly onto the
  // type of interest, as opposed to involving a typeclass.
  implicit def opsAny[A](x: A): Ops.AnyOps[A]                                         = new Ops.AnyOps[A](x)
  implicit def opsArray[A](xs: Array[A]): Ops.ArrayOps[A]                             = new Ops.ArrayOps[A](xs)
  implicit def opsBooleanAlgebra[A](alg: BooleanAlgebra[A]): Ops.BooleanAlgebraOps[A] = new Ops.BooleanAlgebraOps[A](alg)
  implicit def opsCollection[A](x: jAbstractCollection[A]): Ops.jCollectionOps[A]     = new Ops.jCollectionOps(x)
  implicit def opsEq[A](x: Eq[A]): Ops.EqOps[A]                                       = new Ops.EqOps[A](x)
  implicit def opsForeach[A](xs: Foreach[A]): Ops.ForeachOps[A]                       = new Ops.ForeachOps(xs)
  implicit def opsFunction1[T, R](f: T => R): Ops.Function1Ops[T, R]                  = new Ops.Function1Ops[T, R](f)
  implicit def opsGTOnce[A](xs: GTOnce[A]): Ops.GTOnceOps[A]                          = new Ops.GTOnceOps[A](xs)
  implicit def opsInputStream(x: InputStream): Ops.InputStreamOps                     = new Ops.InputStreamOps(x)
  implicit def opsInt(x: Int): Ops.IntOps                                             = new Ops.IntOps(x)
  implicit def opsIterator[A](it: jIterator[A]): Ops.IteratorOps[A]                   = new Ops.IteratorOps(it)
  implicit def opsLong(x: Long): Ops.LongOps                                          = new Ops.LongOps(x)
  implicit def opsMap[K, V](xs: sc.Map[K, V]): Ops.Map[K, V]                          = new Ops.Map[K, V](xs)
  implicit def opsOption[A](x: Option[A]): Ops.OptionOps[A]                           = new Ops.OptionOps[A](x)
  implicit def opsOrder[A](x: Order[A]): Ops.OrderOps[A]                              = new Ops.OrderOps[A](x)
  implicit def opsSeq1[A](xs: sc.Seq[A]): Ops.Seq1[A]                                 = new Ops.Seq1[A](xs)
  implicit def opsSeq2[A](xs: sc.Seq[A]): Ops.Seq2[A]                                 = new Ops.Seq2[A](xs)
  implicit def opsSeqOps[A](xs: sc.Seq[A]): Ops.SeqOps[A]                             = new Ops.SeqOps[A](xs)
  implicit def opsShow[A](x: Show[A]): Ops.ShowOps[A]                                 = new Ops.ShowOps[A](x)
  implicit def opsSizeInfo(x: SizeInfo): Ops.SizeInfoOps                              = new Ops.SizeInfoOps(x)
  implicit def opsSortedMap[K, V](xs: sc.SortedMap[K, V]): Ops.SortedMap[K, V]        = new Ops.SortedMap[K, V](xs)
  implicit def opsTry[A](x: scala.util.Try[A]): Ops.TryOps[A]                         = new Ops.TryOps[A](x)
}

/** An incomplete selection of show compositors.
 *  Not printing the way scala does.
 */
trait ShowImplicits {
  def inBrackets[A: Show](xs: A*): String = Seq("[", xs.toSeq.joinComma, "]") join ""

  implicit def arrayShow[A: Show] : Show[Array[A]]        = Show(xs => inBrackets(xs: _*))
  implicit def booleanShow: Show[Boolean]                 = Show.native()
  implicit def charShow: Show[Char]                       = Show.native()
  implicit def doubleShow: Show[Double]                   = Show.native()
  implicit def indexShow: Show[api.Index]                 = showBy(_.value.to_s)
  implicit def intShow: Show[Int]                         = Show.native()
  implicit def longShow: Show[Long]                       = Show.native()
  implicit def numberShow: Show[ScalaNumber]              = Show.native()
  implicit def optShow[A: Show] : Show[Option[A]]         = Show(_.fold("-")(_.to_s))
  implicit def pspListShow[A: Show] : Show[PspList[A]]    = Show(xs => if (xs.isEmpty) "nil" else (xs join " :: ") + " :: nil")
  implicit def seqShow[A: Show] : Show[Seq[A]]            = Show(xs => inBrackets(xs: _*))
  implicit def showDirect: Show[ShowDirect]               = Show(_.to_s)
  implicit def sizeShow: Show[api.Size]                   = showBy(_.value.to_s)
  implicit def stringShow: Show[String]                   = Show(x => x)
  implicit def tupleShow[A: Show, B: Show] : Show[(A, B)] = Show { case (x, y) => show"$x -> $y" }
  implicit def viewShow[A] : Show[api.View[A]]            = Show(_.viewChain reverseMap (_.description) joinSpace)

  implicit def sizeInfoShow: Show[SizeInfo] = Show[SizeInfo] {
    case Bounded(lo, Infinite) => show"[$lo, <inf>)"
    case Bounded(lo, hi)       => show"[$lo, $hi]"
    case Precise(size)         => show"$size"
    case Infinite              => "<inf>"
  }
}

trait ReadImplicits {
  implicit val bigDecRead: Read[BigDecimal] = Read(s => BigDecimal(s))
  implicit val bigIntRead: Read[BigInt]     = Read(s => BigInt(s))
  implicit val doubleRead: Read[Double]     = Read(_.toDouble)
  implicit val floatRead: Read[Float]       = Read(_.toFloat)
  implicit val intRead: Read[Int]           = Read(_.toInt)
  implicit val longRead: Read[Long]         = Read(_.toLong)
  implicit val stringRead: Read[String]     = Read(s => s)
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
    import PartialOrder._
    import SizeInfo.GenBounded

    def partialCompare(lhs: SizeInfo, rhs: SizeInfo): PCmp = (lhs, rhs) match {
      case (Infinite, Infinite)                     => EQ
      case (Precise(_), Infinite)                   => LT
      case (Infinite, Precise(_))                   => GT
      case (Precise(x), Precise(y))                 => if (x < y) LT else if (y < x) GT else EQ
      case (Infinite, Bounded(_, Infinite))         => GE
      case (Infinite, _)                            => GT
      case (Bounded(_, Infinite), Infinite)         => LE
      case (_, Infinite)                            => LT
      case (GenBounded(l1, h1), GenBounded(l2, h2)) =>
      def lo1 = Precise(l1)
      def lo2 = Precise(l2)

      ( if (h1 < lo2 isTrue) LT
        else if (h1 <= lo2 isTrue) LE
        else if (h2 < lo1 isTrue) GT
        else if (h2 <= lo1 isTrue) GE
        else NA
      )
    }
  }
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
