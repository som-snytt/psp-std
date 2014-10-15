package psp
package std

import java.{ lang => jl }
import scala.{ collection => sc }
import api._
import psp.dmz.PolicyDmz

/** Yes I know all about implicit classes.
 *  There's no way to write an implicit value class which doesn't hardcode
 *  its location into an object. Separating the implicit conversion from
 *  the class allows clients to build their own package object.
 *
 *  This is all a consequence of scala offering no means for managing namespaces,
 *  so namespace management has become hopelessly entangled with unrelated concerns
 *  like inheritance, specificity, method dispatch, and so forth.
 */
abstract class PackageImplicits extends scala.AnyRef
      with StandardImplicits2
      with ShowImplicits
      with ReadImplicits
      with StdOrder
      with StdAlgebra
      with StdGateways
      with StdArrowAssoc
      with PolicyDmz {

  // Promotion of the api type (which generally has one method) to the concrete type
  // which has all the other ones.
  implicit def apiSizePromote(x: Size): Size.Impl               = Size(x.value)
  implicit def apiIndexPromote(x: api.Index): IndexImpl         = Index(x.value)
  implicit def apiNthPromote(x: api.Nth): Nth.Impl              = Nth(x.value)
  implicit def apiOrderPromote[A](ord: Order[A]): Order.Impl[A] = Order(ord.compare)
  implicit def sizeToSizeInfo(s: Size): SizeInfo                = s.toInfo

  // The typesafe non-toString-using show"..." interpolator.
  implicit def opsApiShowInterpolator(sc: StringContext): ShowInterpolator = new ShowInterpolator(sc)

  // Continuing the delicate dance against scala's hostile-to-correctness intrinsics.
  implicit def showableToShown[A: Show](x: A): Shown = Shown(implicitly[Show[A]] show x)

  implicit class ArrayViewOps[A](val repr: Array[A]) {
    def m: IndexedView[A, Array[A]] = new DirectAccess.ArrayIs[A] wrap repr
  }
  implicit class StringViewOps[A](val repr: String) {
    def m: IndexedView[Char, String] = DirectAccess.StringIs wrap repr
  }
  implicit class ApiOrderOps[A](val ord: Order[A]) {
    def reverse: Order[A]          = Order[A]((x, y) => ord.compare(x, y).flip)
    def on[B](f: B => A): Order[B] = Order[B]((x, y) => ord.compare(f(x), f(y)))
  }
  implicit class BuildsOps[Elem, To](z: Builds[Elem, To]) {
    def map[Next](f: To => Next): Builds[Elem, Next] = Builds(xs => f(z build xs))
  }
}

trait StandardImplicits0 {
  // The array operations of last resort. To be eliminated.
  implicit def genericArrayOps[T](xs: Array[T]) = scala.Predef.genericArrayOps[T](xs)
}
trait StandardImplicits1 extends StandardImplicits0 {
  implicit def walkableOps[Repr, A0](repr: Repr)(implicit tc: Walkable[Repr]) = new OpsContainer(() => tc wrap repr)
}
trait StandardImplicits2 extends StandardImplicits1 {
  // We buried Predef's {un,}augmentString in favor of these.
  @inline final implicit def pspAugmentString(x: String): PspStringOps   = new PspStringOps(x)
  @inline final implicit def pspUnaugmentString(x: PspStringOps): String = x.toString

  // Direct-acting extension methods. These are extension methods installed directly onto the
  // type of interest, as opposed to involving a typeclass.
  implicit def opsArray[A](xs: Array[A]): Ops.ArrayOps[A]                         = new Ops.ArrayOps[A](xs)
  implicit def opsChar(x: Char): ops.CharOps                                      = new ops.CharOps(x)
  implicit def opsCollection[A](x: jAbstractCollection[A]): Ops.jCollectionOps[A] = new Ops.jCollectionOps(x)
  implicit def opsForeach[A](xs: Foreach[A]): Ops.ForeachOps[A]                   = new Ops.ForeachOps(xs)
  implicit def opsFunction1[T, R](f: T => R): Ops.Function1Ops[T, R]              = new Ops.Function1Ops[T, R](f)
  implicit def opsGTOnce[A](xs: GTOnce[A]): Ops.GTOnceOps[A]                      = new Ops.GTOnceOps[A](xs)
  implicit def opsInputStream(x: InputStream): Ops.InputStreamOps                 = new Ops.InputStreamOps(x)
  implicit def opsInt(x: Int): ops.IntOps                                         = new ops.IntOps(x)
  implicit def opsIterator[A](it: jIterator[A]): Ops.IteratorOps[A]               = new Ops.IteratorOps(it)
  implicit def opsLong(x: Long): ops.LongOps                                      = new ops.LongOps(x)
  implicit def opsMap[K, V](xs: scMap[K, V]): Ops.Map[K, V]                       = new Ops.Map[K, V](xs)
  implicit def opsOption[A](x: Option[A]): Ops.OptionOps[A]                       = new Ops.OptionOps[A](x)
  implicit def opsSeq1[A](xs: scSeq[A]): Ops.Seq1[A]                              = new Ops.Seq1[A](xs)
  implicit def opsSeq2[A](xs: scSeq[A]): Ops.Seq2[A]                              = new Ops.Seq2[A](xs)
  implicit def opsSeqOps[A](xs: scSeq[A]): Ops.SeqOps[A]                          = new Ops.SeqOps[A](xs)
  implicit def opsSizeInfo(x: SizeInfo): Ops.SizeInfoOps                          = new Ops.SizeInfoOps(x)
  implicit def opsSortedMap[K, V](xs: sc.SortedMap[K, V]): Ops.SortedMap[K, V]    = new Ops.SortedMap[K, V](xs)
  implicit def opsTry[A](x: scala.util.Try[A]): Ops.TryOps[A]                     = new Ops.TryOps[A](x)
}

trait StdAlgebra {
  implicit def identityAlgebra : BooleanAlgebra[Boolean]          = Algebras.Identity
  implicit def predicateAlgebra[A] : BooleanAlgebra[Predicate[A]] = new Algebras.Predicate[A]
  implicit def scalaSetAlgebra[A] : BooleanAlgebra[sciSet[A]]     = new Algebras.ScalaSet[A]

  implicit def opsBooleanAlgebra[A](x: BooleanAlgebra[A]): ops.BooleanAlgebraOps[A] = new ops.BooleanAlgebraOps[A](x)
}

trait ReadImplicits {
  implicit val bigDecRead: Read[BigDecimal] = Read(s => BigDecimal(s))
  implicit val bigIntRead: Read[BigInt]     = Read(s => BigInt(s))
  implicit val doubleRead: Read[Double]     = Read(_.toDouble)
  implicit val floatRead: Read[Float]       = Read(_.toFloat)
  implicit val intRead: Read[Int]           = Read(_.toInt)
  implicit val longRead: Read[Long]         = Read(_.toLong)
  implicit val stringRead: Read[String]     = Read(s => s)
  implicit val uriRead: Read[jUri]          = Read(jUri)
  implicit val regexRead: Read[Regex]       = Read(Regex)
}

trait StdOrder {
  implicit def booleanOrder: Order[Boolean] = orderBy[Boolean](x => if (x) 1 else 0)
  implicit def byteOrder: Order[Byte]       = Order.fromInt[Byte](_ - _)
  implicit def charOrder: Order[Char]       = Order.fromInt[Char](_ - _)
  implicit def intOrder: Order[Int]         = Order.fromInt[Int](_ - _)
  implicit def longOrder: Order[Long]       = Order.fromLong[Long](_ - _)
  implicit def shortOrder: Order[Short]     = Order.fromInt[Short](_ - _)
  implicit def stringOrder: Order[String]   = Order.fromLong[String](_ compareTo _)

  implicit def indexOrder: Order[Index] = orderBy[Index](_.indexValue)
  implicit def sizeOrder: Order[Size]   = orderBy[Size](_.value)

  implicit def tuple2Order[A: Order, B: Order] : Order[(A, B)]              = Order[(A, B)]((x, y) => Order.fold(x._1 compare y._1, x._2 compare y._2))
  implicit def tuple3Order[A: Order, B: Order, C: Order] : Order[(A, B, C)] = Order[(A, B, C)]((x, y) => Order.fold(x._1 compare y._1, x._2 compare y._2, x._3 compare y._3))

  implicit def sizeInfoPartialOrder: PartialOrder[SizeInfo] = PartialOrder(SizeInfo.partialCompare)
}

trait StdEq {
  implicit def booleanEq: Eq[Boolean] = Eq.natural[Boolean]
  implicit def byteEq: Eq[Byte]       = Eq.natural[Byte]
  implicit def charEq: Eq[Char]       = Eq.natural[Char]
  implicit def doubleEq: Eq[Double]   = Eq.natural[Double]
  implicit def floatEq: Eq[Float]     = Eq.natural[Float]
  implicit def intEq: Eq[Int]         = Eq.natural[Int]
  implicit def longEq: Eq[Long]       = Eq.natural[Long]
  implicit def shortEq: Eq[Short]     = Eq.natural[Short]
  implicit def stringEq: Eq[String]   = Eq.natural[String]
  implicit def unitEq: Eq[Unit]       = Eq[Unit]((x, y) => true)

  implicit def sizeEq: Eq[Size]         = eqBy[Size](_.value)
  implicit def indexEq: Eq[Index]       = eqBy[Index](_.indexValue)
  implicit def sizeInfoEq: Eq[SizeInfo] = Eq.natural()
  implicit def jTypeEq: Eq[jType]       = Eq.natural()

  implicit def setEq[A: HashEq] : Eq[scSet[A]]            = Eq(_ sameMembers _)
  implicit def seqEq[A: Eq] : Eq[scSeq[A]]                = Eq((xs, ys) => (xs corresponds ys)(_ === _))
  implicit def directEq[A: Eq] : Eq[Direct[A]]            = Eq((xs, ys) => (xs.size == ys.size) && (xs.size.toIndexRange forall (i => xs.elemAt(i) === ys.elemAt(i))))
  implicit def arrayEq[A: Eq] : Eq[Array[A]]              = eqBy[Array[A]](_.toSeq)
  implicit def mapEq[K: HashEq, V: Eq] : Eq[sciMap[K, V]] = Eq((xs, ys) => (xs.keys sameMembers ys.keys) && (xs.keys forall (xs sameAt ys)))

  implicit def equivFromOrder[A: Order] : Eq[A] = Eq[A]((x, y) => (x compare y) eq Cmp.EQ)
}

trait StdHash {
  implicit def booleanHash: Hash[Boolean] = Hash.natural()
  implicit def byteHash: Hash[Byte]       = Hash.natural()
  implicit def charHash: Hash[Char]       = Hash.natural()
  implicit def doubleHash: Hash[Double]   = Hash.natural()
  implicit def floatHash: Hash[Float]     = Hash.natural()
  implicit def intHash: Hash[Int]         = Hash.natural()
  implicit def longHash: Hash[Long]       = Hash.natural()
  implicit def shortHash: Hash[Short]     = Hash.natural()
  implicit def stringHash: Hash[String]   = Hash.natural()
  implicit def unitHash: Hash[Unit]       = Hash.natural()
  implicit def pathHash: Hash[Path]       = Hash.natural()
  implicit def jTypeHash: Hash[jType]     = Hash.natural()

  implicit def sizeHash: Hash[Size]   = hashBy[Size](_.value)
  implicit def indexHash: Hash[Index] = hashBy[Index](_.indexValue)

  implicit def seqHash[A: Hash] : Hash[scSeq[A]]     = Hash[scSeq[A]](xs => xs.map(_.hash).##)
  implicit def directHash[A: Hash] : Hash[Direct[A]] = hashBy(_.toScalaVector)
  implicit def arrayHash[A: Hash] : Hash[Array[A]]   = hashBy[Array[A]](_.m.toScalaVector)
}

object StdEq extends StdEq
object StdHash extends StdHash
