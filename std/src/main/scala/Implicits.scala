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
abstract class StdPackage
      extends StdLabel
         with StdOrder
         with StdZipped
         with StdProperties
         with StdAlgebra
         with StdGateways
         with StdArrowAssoc
         with PolicyDmz {

  implicit class ApiOrderOps[A](val ord: Order[A]) {
    def reverse: Order[A]          = Order[A]((x, y) => ord.compare(x, y).flip)
    def on[B](f: B => A): Order[B] = Order[B]((x, y) => ord.compare(f(x), f(y)))
  }
  implicit class BuildsOps[Elem, To](z: Builds[Elem, To]) {
    def comap[Prev](f: Prev => Elem): Builds[Prev, To] = Builds(xs => z build (xs map f))
    def map[Next](f: To => Next): Builds[Elem, Next]   = Builds(xs => f(z build xs))
  }
  implicit class IndexLikeOps[A](val x: IndexLike { type This = A }) {
    def -(n: Int): A = x + (-n)
    def prev: A      = x + (-1)
    def next: A      = x + 1
  }
  implicit class JavaEnumerationOps[A](it: jEnumeration[A]) {
    def toIterator = BiIterator enumeration it
  }
  implicit def conforms[A] : (A <:< A) = new conformance[A]

  implicit def intensionalToFunction1[T, R](f: Intensional[T, R]): T => R = x => f(x)
}

trait StdLabel0 {
  implicit def functionIsLabelable[T, R] : Labelable[T => R] = new Labelable[T => R] {
    def label(f: T => R, label: String) = new LabeledFunction(f, label)
  }
}
trait StdLabel extends StdLabel0 {
  implicit def partialFunctionIsLabelable[T, R] : Labelable[T ?=> R] = new Labelable[T ?=> R] {
    def label(f: T ?=> R, label: String) = new LabeledPartialFunction(f, label)
  }
  implicit def viewIsLabelable[A, Repr] = new api.Labelable[BaseView[A, Repr]] {
    def label(x: BaseView[A, Repr], label: String) = new LabeledView[A, Repr](x, label)
  }
}

trait StdAlgebra {
  implicit def identityAlgebra : BooleanAlgebra[Boolean]          = Algebras.Identity
  implicit def scalaLabelAlgebra : BooleanAlgebra[Label]          = Algebras.LabelAlgebra
  implicit def predicateAlgebra[A] : BooleanAlgebra[Predicate[A]] = new Algebras.Predicate[A]
  // TODO
  // implicit def policySetAlgebra[A] : BooleanAlgebra[pSet[A]]   = new Algebras.PolicySet[A]

  implicit def opsBooleanAlgebra[A](x: BooleanAlgebra[A]): ops.BooleanAlgebraOps[A] = new ops.BooleanAlgebraOps[A](x)
}

trait StdRead {
  implicit def bigDecRead: Read[BigDecimal] = Read(s => BigDecimal(s))
  implicit def bigIntRead: Read[BigInt]     = Read(s => BigInt(s))
  implicit def doubleRead: Read[Double]     = Read(_.toDouble)
  implicit def floatRead: Read[Float]       = Read(_.toFloat)
  implicit def intRead: Read[Int]           = Read(_.toInt)
  implicit def longRead: Read[Long]         = Read(_.toLong)
  implicit def stringRead: Read[String]     = Read(s => s)
  implicit def uriRead: Read[jUri]          = Read(jUri)
  implicit def regexRead: Read[Regex]       = Read(Regex)
}

trait StdOrder {
  implicit def booleanOrder: Order[Boolean] = orderBy[Boolean](x => if (x) 1 else 0)
  implicit def byteOrder: Order[Byte]       = Order.fromInt[Byte](_ - _)
  implicit def charOrder: Order[Char]       = Order.fromInt[Char](_ - _)
  implicit def intOrder: Order[Int]         = Order.fromInt[Int](_ - _)
  implicit def longOrder: Order[Long]       = Order.fromLong[Long](_ - _)
  implicit def shortOrder: Order[Short]     = Order.fromInt[Short](_ - _)
  implicit def stringOrder: Order[String]   = Order.fromLong[String](_ compareTo _)

  implicit def indexOrder: Order[Index]      = orderBy[Index](_.indexValue)
  implicit def nthOrder: Order[Nth]          = orderBy[Nth](_.nthValue)
  implicit def offsetOrder: Order[Offset]    = orderBy[Offset](_.offsetValue)
  implicit def sizeOrder: Order[PreciseSize] = orderBy[PreciseSize](_.value)

  implicit def tuple2Order[A: Order, B: Order] : Order[(A, B)]              = Order[(A, B)]((x, y) => Order.fold(x._1 compare y._1, x._2 compare y._2))
  implicit def tuple3Order[A: Order, B: Order, C: Order] : Order[(A, B, C)] = Order[(A, B, C)]((x, y) => Order.fold(x._1 compare y._1, x._2 compare y._2, x._3 compare y._3))

  implicit def sizeInfoPartialOrder: PartialOrder[SizeInfo] = PartialOrder(SizeInfo.partialCompare)
}

trait StdZero {
  implicit def unitZero           = Zero[Unit](())
  implicit def stringZero         = Zero[String]("")
  implicit def booleanZero        = Zero[Boolean](false)
  implicit def byteZero           = Zero[Byte](0.toByte)
  implicit def shortZero          = Zero[Short](0.toShort)
  implicit def intZero            = Zero[Int](0)
  implicit def longZero           = Zero[Long](0l)
  implicit def charZero           = Zero[Char](0.toChar)
  implicit def floatZero          = Zero[Float](0f)
  implicit def doubleZero         = Zero[Double](0d)
  implicit def bigIntZero         = Zero[BigInt](BigInt(0))
  implicit def bigDecimalZero     = Zero[BigDecimal](BigDecimal(0))
  implicit def arrayZero[A: CTag] = Zero[Array[A]](Array[A]())
  implicit def iteratorZero[A]    = Zero[sIterator[A]](scIterator.empty)
  implicit def iterableZero[A]    = Zero[sIterable[A]](Nil)
  implicit def listZero[A]        = Zero[sciList[A]](Nil)
  implicit def mapZero[A,B]       = Zero[scMap[A,B]](sciMap())
  implicit def optionZero[A]      = Zero[Option[A]](None)
  implicit def seqZero[A]         = Zero[scSeq[A]](Nil)
  implicit def setZero[A]         = Zero[scSet[A]](sciSet())
  implicit def traversableZero[A] = Zero[scTraversable[A]](Nil)
  implicit def vectorZero[A]      = Zero[sciVector[A]](Vector())

  implicit def indexZero[A]      = Zero[Index](NoIndex)
}

trait StdEq {
  implicit def booleanEq: HashEq[Boolean] = HashEq.natural()
  implicit def byteEq: HashEq[Byte]       = HashEq.natural()
  implicit def charEq: HashEq[Char]       = HashEq.natural()
  implicit def doubleEq: HashEq[Double]   = HashEq.natural()
  implicit def floatEq: HashEq[Float]     = HashEq.natural()
  implicit def intEq: HashEq[Int]         = HashEq.natural()
  implicit def longEq: HashEq[Long]       = HashEq.natural()
  implicit def shortEq: HashEq[Short]     = HashEq.natural()
  implicit def unitHash: HashEq[Unit]     = HashEq.natural()
  implicit def stringEq: HashEq[String]   = HashEq.natural()

  implicit def sizeInfoEq: Eq[SizeInfo] = Eq.natural()
  implicit def sizeEq: Eq[PreciseSize]  = eqBy[PreciseSize](_.value)
  implicit def indexEq: Eq[Index]       = eqBy[Index](_.indexValue)
  implicit def nthEq: Eq[Nth]           = eqBy[Nth](_.nthValue)
  implicit def offsetEq: Eq[Offset]     = eqBy[Offset](_.offsetValue)
  implicit def pathEq: Eq[Path]         = eqBy[Path](_.toString)
  implicit def jTypeEq: Eq[jType]       = Eq.natural()

  implicit def equivFromOrder[A: Order] : Eq[A] = Eq[A]((x, y) => (x compare y) eq Cmp.EQ)

  implicit def pathHash: Hash[Path]       = Hash.natural()
  implicit def jTypeHash: Hash[jType]     = Hash.natural()

  implicit def sizeHash: Hash[PreciseSize] = hashBy[PreciseSize](_.value)
  implicit def indexHash: Hash[Index]      = hashBy[Index](_.indexValue)
  implicit def nthHash: Hash[Nth]          = hashBy[Nth](_.nthValue)
  implicit def offsetHash: Hash[Offset]    = hashBy[Offset](_.offsetValue)

  implicit def exSetEq[A] : Eq[exSet[A]]        = Eq((xs, ys) => (xs isSubsetOf ys) && (ys isSubsetOf xs))
  implicit def seqEq[A: Eq] : HashEq[scSeq[A]]  = HashEq[scSeq[A]]((x, y) => (x corresponds y)(_ === _), _.##)
  implicit def directEq[A: Eq] : Eq[pVector[A]] = Eq((xs, ys) => (xs hasSameSize ys) && (xs.indices forall (i => xs(i) === ys(i))))
  implicit def arrayEq[A: Eq] : Eq[Array[A]]    = eqBy[Array[A]](_.pvec)

  // implicit def mapEq[K: HashEq, V: Eq] : Eq[sciMap[K, V]] = Eq((xs, ys) => (xs.keys sameMembers ys.keys) && (xs.keys forall (xs sameAt ys)))
  // implicit def tupleEq[A: Eq, B: Eq] : Eq[(A, B)]         = Eq((x, y) => (x._1 === y._1) && (x._2 === y._2))

  implicit def tupleHash[A: HashEq, B: HashEq] : HashEq[(A, B)] =
    HashEq[(A, B)]((x, y) => (x._1 === y._1) && (x._2 === y._2), x => x._1.hash + x._2.hash)
}

object StdZero extends StdZero
object StdEq extends StdEq
