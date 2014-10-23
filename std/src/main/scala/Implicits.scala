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
      extends OrderInstances
         with StdZipped
         with StdProperties
         with AlgebraInstances
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
    def direct: Suspended[Elem] => To                  = mf => z build Foreach(mf)
    def scalaBuilder: scmBuilder[Elem, To]                = sciVector.newBuilder[Elem] mapResult (xs => z build xs)
  }
  implicit class JavaEnumerationOps[A](it: jEnumeration[A]) {
    def toIterator = BiIterator enumeration it
  }
  implicit class TupleViewOps[A, B](val xs: View[(A, B)]) {
    def filterLeft(p: Predicate[A])  = xs withFilter (x => p(x._1))
    def filterRight(p: Predicate[B]) = xs withFilter (x => p(x._2))
    def lefts: View[A]               = xs map (_._1)
    def rights: View[B]              = xs map (_._2)
  }
  implicit class Tuple2Ops[A, B](val lhs: (A, B)) {
    def fold[C, D](rhs: (A, B))(f: (A, A) => C, g: (B, B) => C)(h: (C, C) => D): D =
      h(f(lhs._1, rhs._1), g(lhs._2, rhs._2))
  }
  implicit class AnyTargetSeqOps[A: HashEq](root: A) {
    def transitiveClosure(expand: A => pSeq[A]): pSeq[A] = {
      var seen = PolicySet.elems[A]()
      def loop(root: A, f: A => Unit): Unit = if (!seen(root)) {
        seen = seen union exSet(root)
        f(root)
        expand(root) |> (xs => if (xs != null) xs foreach (x => loop(x, f)))
      }
      Foreach(f => loop(root, f))
    }
  }
  implicit def booleanToPredicate(value: Boolean): Predicate[Any] = if (value) ConstantTrue else ConstantFalse
  implicit def jClassToPolicyClass(x: jClass): PolicyClass        = new PolicyClass(x)

  implicit def viewifyString(x: String): View[Char]          = x.m
  implicit def viewifyArray[A](x: Array[A]): View[A]         = x.m[DirectAccess] // must give this type argument explicitly.
  implicit def unViewifyString(x: View[Char]): String        = x.force[String]
  implicit def unViewifyArray[A: CTag](x: View[A]): Array[A] = x.force[Array[A]]

  implicit def convertPolicySeq[A, B](xs: pSeq[A])(implicit conversion: A => B): pSeq[B] = xs map (x => conversion(x))
  implicit def scalaSeqToPSeq[A](x: scSeq[A]): pVector[A] = x.pvec

  implicit def conforms[A] : (A <:< A) = new conformance[A]
}

trait AlgebraInstances {
  implicit def identityAlgebra : BooleanAlgebra[Boolean]           = Algebras.Identity
  implicit def predicateAlgebra[A] : BooleanAlgebra[Predicate[A]]  = new Algebras.Predicate[A]
  implicit def intensionalSetAlgebra[A] : BooleanAlgebra[inSet[A]] = new Algebras.inSetAlgebra[A]
}

trait ReadInstances {
  implicit def bigDecRead: Read[BigDecimal] = Read(s => BigDecimal(s))
  implicit def bigIntRead: Read[BigInt]     = Read(s => BigInt(s))
  implicit def doubleRead: Read[Double]     = Read(_.toDouble)
  implicit def floatRead: Read[Float]       = Read(_.toFloat)
  implicit def intRead: Read[Int]           = Read(_.toInt)
  implicit def longRead: Read[Long]         = Read(_.toLong)
  implicit def regexRead: Read[Regex]       = Read(Regex)
  implicit def stringRead: Read[String]     = Read(s => s)
  implicit def uriRead: Read[jUri]          = Read(jUri)
}

trait OrderInstances {
  implicit def booleanOrder: Order[Boolean] = orderBy[Boolean](x => if (x) 1 else 0)
  implicit def byteOrder: Order[Byte]       = Order.fromInt[Byte](_ - _)
  implicit def charOrder: Order[Char]       = Order.fromInt[Char](_ - _)
  implicit def intOrder: Order[Int]         = Order.fromInt[Int](_ - _)
  implicit def longOrder: Order[Long]       = Order.fromLong[Long](_ - _)
  implicit def shortOrder: Order[Short]     = Order.fromInt[Short](_ - _)
  implicit def stringOrder: Order[String]   = Order.fromLong[String](_ compareTo _)

  implicit def indexOrder: Order[Index]              = orderBy[Index](_.indexValue)
  implicit def nthOrder: Order[Nth]                  = orderBy[Nth](_.nthValue)
  implicit def offsetOrder: Order[Offset]            = orderBy[Offset](_.offsetValue)
  implicit def preciseOrder[A <: Precise] : Order[A] = orderBy[A](_.value)

  implicit def tuple2Order[A: Order, B: Order] : Order[(A, B)]              = Order[(A, B)]((x, y) => Order.fold(x._1 compare y._1, x._2 compare y._2))
  implicit def tuple3Order[A: Order, B: Order, C: Order] : Order[(A, B, C)] = Order[(A, B, C)]((x, y) => Order.fold(x._1 compare y._1, x._2 compare y._2, x._3 compare y._3))

  implicit def sizePartialOrder: PartialOrder[Size] = PartialOrder(Size.partialCompare)
}

trait MonoidInstances {
  import StdZero._

  implicit def seqAddition[A] : Sums[pSeq[A]]       = Sums(_ ++ _)
  implicit def vectorAddition[A] : Sums[pVector[A]] = Sums(_ ++ _)
}

trait ZeroInstances {
  implicit def arrayZero[A: CTag] : Zero[Array[A]]           = Zero(Array[A]())
  implicit def bigDecimalZero: Zero[BigDecimal]              = Zero(BigDecimal(0))
  implicit def bigIntZero: Zero[BigInt]                      = Zero(BigInt(0))
  implicit def booleanZero: Zero[Boolean]                    = Zero(false)
  implicit def byteZero: Zero[Byte]                          = Zero(0.toByte)
  implicit def charZero: Zero[Char]                          = Zero(0.toChar)
  implicit def doubleZero: Zero[Double]                      = Zero(0d)
  implicit def floatZero: Zero[Float]                        = Zero(0f)
  implicit def indexZero: Zero[Index]                        = Zero(NoIndex)
  implicit def intZero: Zero[Int]                            = Zero(0)
  implicit def longZero: Zero[Long]                          = Zero(0L)
  implicit def pseqZero[A] : Zero[pSeq[A]]                   = Zero(Foreach.elems())
  implicit def pVectorZero[A] : Zero[pVector[A]]             = Zero(Direct())
  implicit def optionZero[A] : Zero[Option[A]]               = Zero(None)
  implicit def scIterableZero[A] : Zero[scIterable[A]]       = Zero(Nil)
  implicit def scIteratorZero[A] : Zero[scIterator[A]]       = Zero(scIterator.empty)
  implicit def scSeqZero[A] : Zero[scSeq[A]]                 = Zero(Nil)
  implicit def scSetZero[A] : Zero[scSet[A]]                 = Zero(sciSet())
  implicit def scTraversableZero[A] : Zero[scTraversable[A]] = Zero(Nil)
  implicit def sciListZero[A] : Zero[sciList[A]]             = Zero(Nil)
  implicit def sciMapZero[A, B] : Zero[sciMap[A, B]]         = Zero(sciMap())
  implicit def sciVectorZero[A] : Zero[sciVector[A]]         = Zero(sciVector())
  implicit def shortZero: Zero[Short]                        = Zero(0.toShort)
  implicit def stringZero: Zero[String]                      = Zero("")
  implicit def unitZero: Zero[Unit]                          = Zero(())
}

trait EqInstances {
  import HashEq.natural

  implicit def booleanEq: HashEq[Boolean] = natural()
  implicit def byteEq: HashEq[Byte]       = natural()
  implicit def charEq: HashEq[Char]       = natural()
  implicit def doubleEq: HashEq[Double]   = natural()
  implicit def floatEq: HashEq[Float]     = natural()
  implicit def intEq: HashEq[Int]         = natural()
  implicit def longEq: HashEq[Long]       = natural()
  implicit def shortEq: HashEq[Short]     = natural()
  implicit def unitHash: HashEq[Unit]     = natural()

  implicit def indexEq: HashEq[Index]             = natural()
  implicit def jTypeEq: HashEq[jType]             = natural()
  implicit def nthEq: HashEq[Nth]                 = natural()
  implicit def offsetEq: HashEq[Offset]           = natural()
  implicit def stringEq: HashEq[String]           = natural()
  implicit def policyClassEq: HashEq[PolicyClass] = natural()

  implicit def sizeEq: HashEq[Size] = HashEq(Size.equiv, Size.hash)
  implicit def pathEq: HashEq[Path] = hashEqBy[Path](_.toString)

  implicit def tryEq[A](implicit z1: Eq[A], z2: Eq[Throwable]): Eq[Try[A]] = Eq {
    case (Success(x), Success(y)) => x === y
    case (Failure(x), Failure(y)) => x === y
    case _                        => false
  }

  // Since Sets are created with their own notion of equality, you can't pass
  // an Eq instance. Map keys are also a set.
  implicit def arrayHashEq[A: HashEq] : HashEq[Array[A]]       = hashEqBy[Array[A]](_.pvec)
  implicit def vectorHashEq[A: Eq] : HashEq[pVector[A]]        = HashEq(corresponds[A], _.toScalaVector.##)
  implicit def exSetEq[A] : Eq[exSet[A]]                       = Eq(symmetrically[exSet[A]](_ isSubsetOf _))
  implicit def pMapEq[K, V: Eq] : Eq[pMap[K, V]]               = Eq((xs, ys) => xs.keySet === ys.keySet && (equalizer(xs.apply, ys.apply) forall xs.keys))
  implicit def tuple2Eq[A: HashEq, B: HashEq] : HashEq[(A, B)] = HashEq[(A, B)]({ case ((x1, y1), (x2, y2)) => x1 === x2 && y1 === y2 }, x => x._1.hash + x._2.hash)

  implicit def equivFromOrder[A: Order] : Eq[A] = Eq[A](_ compare _ eq Cmp.EQ)

  def equalizer[A, B: Eq](f: A => B, g: A => B): FunctionEqualizer[A, B] = new FunctionEqualizer(f, g)
  def symmetrically[A](f: Relation[A]): Relation[A]                      = (x, y) => f(x, y) && f(y, x)
}
