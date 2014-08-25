package psp
package std

import java.{ lang => jl }
import scala.{ collection => sc }
import Trilean._

object TClass {
  final class OrderOps[A](private val lhs: A) extends AnyVal {
    import Order.Cmp, Order.Cmp._
    def compare(rhs: A)(implicit ord: Order[A]): Cmp = ord.compare(lhs, rhs)
    def < (rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) == LT
    def <=(rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) != GT
    def >=(rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) != LT
    def >(rhs: A)(implicit ord: Order[A]): Boolean   = compare(rhs) == GT
    def min(rhs: A)(implicit ord: Order[A]): A       = if (compare(rhs) == LT) lhs else rhs
    def max(rhs: A)(implicit ord: Order[A]): A       = if (compare(rhs) == GT) lhs else rhs
  }
  final class PartialOrderOps[A](private val lhs: A) extends AnyVal {
    import PartialOrder._
    private def evaluate(rhs: A)(trues: Cmp*)(undefs: Cmp*)(implicit ord: PartialOrder[A]): Trilean = {
      val res = partialCompare(rhs)
      if (trues contains res) True
      else if (undefs contains res) Undefined
      else False
    }
    def partialCompare(rhs: A)(implicit ord: PartialOrder[A]): Cmp = ord.partialCompare(lhs, rhs)
    def < (rhs: A)(implicit ord: PartialOrder[A]): Trilean         = evaluate(rhs)(LT)(NA, LE)
    def <=(rhs: A)(implicit ord: PartialOrder[A]): Trilean         = evaluate(rhs)(LT, LE, EQ)(NA)
    def >=(rhs: A)(implicit ord: PartialOrder[A]): Trilean         = evaluate(rhs)(GT, GE, EQ)(NA)
    def >(rhs: A)(implicit ord: PartialOrder[A]): Trilean          = evaluate(rhs)(GT)(NA, GE)
    def min(rhs: A)(implicit ord: PartialOrder[A]): Option[A]      = partialCompare(rhs) match { case LT | LE => Some(lhs) ; case GT | GE => Some(rhs) ; case _ => None }
    def max(rhs: A)(implicit ord: PartialOrder[A]): Option[A]      = partialCompare(rhs) match { case LT | LE => Some(rhs) ; case GT | GE => Some(lhs) ; case _ => None }
  }
  final class AlgebraOps[A](private val lhs: A) extends AnyVal {
    def implies(rhs: A)(implicit alg: BooleanAlgebra[A]) = !lhs || rhs
    def && (rhs: A)(implicit alg: BooleanAlgebra[A]): A  = if (isOne) rhs else if (rhs.isOne) lhs else if (lhs.isZero || rhs.isZero) alg.zero else alg.and(lhs, rhs)
    def || (rhs: A)(implicit alg: BooleanAlgebra[A]): A  = if (isZero) rhs else if (rhs.isZero) lhs else if (lhs.isOne || rhs.isOne) alg.one else alg.or(lhs, rhs)
    def unary_!(implicit alg: BooleanAlgebra[A]): A      = if (isZero) alg.one else if (isOne) alg.zero else alg.not(lhs)
    def isZero(implicit alg: BooleanAlgebra[A]): Boolean = alg.zero ref_== lhs
    def isOne(implicit alg: BooleanAlgebra[A]): Boolean  = alg.one ref_== lhs
  }
  final class ShowDirectOps(private val x: ShowDirect) extends AnyVal {
    /** Java-style String addition without abandoning type safety.
     */
    def + (that: ShowDirect): ShowDirect = Shown(show"$x$that")
    def + [A: Show](that: A): ShowDirect = Shown(show"$x$that")
  }
}

object Ops {
  // Have to each go into their own class because the apply methods have the same erasure.
  final class Seq1[CC[X] <: sc.Seq[X], A](private val xs: CC[A]) extends AnyVal {
    def apply(nth: Nth): A = if (nth.isUndefined) sys.error(s"apply($nth)") else xs(nth.intIndex)
  }
  final class Seq2[CC[X] <: sc.Seq[X], A](private val xs: CC[A]) extends AnyVal {
    def apply(index: Index): A = if (index.isUndefined) sys.error(s"apply($index)") else xs(index.value)
  }
  final class Seq[CC[X] <: sc.Seq[X], A](private val xs: CC[A]) extends AnyVal with SeqLikeOps[A] {
    def length                                   = xs.length
    def index(elem: A): Index                    = Index(xs indexOf elem)
    def lastIndex(elem: A): Index                = Index(xs lastIndexOf elem)
    def indexAtWhich(p: A => Boolean): Index     = Index(xs indexWhere p)
    def lastIndexAtWhich(p: A => Boolean): Index = Index(xs lastIndexWhere p)
    def hasElem(elem: A): Boolean                = xs contains elem
    // Produces a vector containing the elements in the given index range.
    // Ignores indices which don't exist in the target sequence.
    def apply(range: IndexRange): Vector[A] = indexRange intersect range map (i => xs(i.value))
  }
  final class Map[K, V](private val map: sc.Map[K, V]) extends AnyVal {
    def sortedKeys(implicit ord: Ordering[K])                     = map.keys.toSeq.sorted
    def orderByKey(implicit ord: Ordering[K]): OrderedMap[K, V]   = orderedMap(sortedKeys, map.toMap)
    def orderByValue(implicit ord: Ordering[V]): OrderedMap[K, V] = orderedMap(sortedKeys(ord on map), map.toMap)
  }
  final class SortedMap[K, V](private val map: sc.SortedMap[K, V]) extends AnyVal {
    private def ord = map.ordering
    def reverse: OrderedMap[K, V] = map orderByKey ord.reverse
  }
  final class GTOnce[CC[X] <: sc.GenTraversableOnce[X], A](private val xs: CC[A]) extends AnyVal {
    def findOr(p: A => Boolean, alt: => A): A                              = (xs find p) | alt
    def sortDistinct(implicit ord: Ordering[A]): Vector[A]                 = distinct.sorted
    def mapZip[B, C](ys: GenTraversableOnce[B])(f: (A, B) => C): Vector[C] = for ((x, y) <- xs.toVector zip ys.toVector) yield f(x, y)
    def mapFrom[B](f: A => B): OrderedMap[B, A]                            = orderedMap(xs.toVector map (x => (f(x), x)): _*)
    def mapOnto[B](f: A => B): OrderedMap[A, B]                            = orderedMap(xs.toVector map (x => (x, f(x))): _*)
    def mapToAndOnto[B, C](k: A => B, v: A => C): OrderedMap[B, C]         = xs.toVector |> (xs => orderedMap(xs map (x => k(x) -> v(x)): _*))
    def mapToMapPairs[B, C](f: A => (B, C)): OrderedMap[B, C]              = xs.toVector |> (xs => orderedMap(xs map f: _*))
    def sorted(implicit ord: Ordering[A]): Vector[A]                       = xs.toVector.sorted
    def distinct: Vector[A]                                                = xs.toVector.distinct
    def unsortedFrequencyMap: Map[A, Int]                                  = immutableMap(xs.toVector groupBy (x => x) mapValues (_.size) toSeq: _*)
    def ascendingFrequency: OrderedMap[A, Int]                             = unsortedFrequencyMap |> (_.orderByValue)
    def descendingFrequency: OrderedMap[A, Int]                            = ascendingFrequency.reverse
  }

  /** We could avoid this duplication with some of scala's incredibly high-cost
   *  (in performance and complexity tax) machinery. I'll take the duplication at this point.
   */
  trait SeqLikeOps[A] extends Any {
    // An added benefit of these methods is a huge increase in type safety
    // because the sequence is treated as invariant due to an idiosyncrasy of
    // scala's type inference.
    def length: Int
    def index(elem: A): Index
    def lastIndex(elem: A): Index
    def indexAtWhich(p: A => Boolean): Index
    def lastIndexAtWhich(p: A => Boolean): Index
    def hasElem(elem: A): Boolean

    def hasIndex(index: Index): Boolean = indexRange contains index
    def indexRange: IndexRange          = IndexRange zeroUntil exclusiveEnd
    def exclusiveEnd: Index             = Index(length)
  }
  final class ArrayOps[A](private val xs: Array[A]) extends AnyVal with SeqLikeOps[A] {
    def length                                   = xs.length
    def index(elem: A): Index                    = Index(xs indexOf elem)
    def lastIndex(elem: A): Index                = Index(xs lastIndexOf elem)
    def indexAtWhich(p: A => Boolean): Index     = Index(xs indexWhere p)
    def lastIndexAtWhich(p: A => Boolean): Index = Index(xs lastIndexWhere p)
    def hasElem(elem: A): Boolean                = xs contains elem

    def apply(range: IndexRange)(implicit tag: ClassTag[A]): Array[A] = (indexRange intersect range) |> (r => xs.slice(r.startInt, r.endInt))
  }

  final class OrderOps[A](private val ord: Order[A]) extends AnyVal {
    def on[B](f: B => A): Order[B] = Order[B]((x, y) => ord.compare(f(x), f(y)))
  }
  final class ShowOps[A](private val shows: Show[A]) extends AnyVal {
    def on[B](f: B => A): Show[B] = Show[B](x => shows show f(x))
  }
  final class EqOps[A](private val eqs: Eq[A]) extends AnyVal {
    def on[B](f: B => A): Eq[B] = Eq[B]((x, y) => eqs.equiv(f(x), f(y)))
  }

  final class AnyOps[A](private val x: A) extends AnyVal {
    // The famed forward pipe.
    @inline def |>[B](f: A => B): B = f(x)

    // "Maybe we can enforce good programming practice with annoyingly long method names."
    def castTo[U] : U         = x.asInstanceOf[U]
    def toRef: AnyRef         = castTo[AnyRef]
    @inline def doto(f: A => Unit): A = try x finally f(x)
    @inline def also(body: Unit): A   = x

    // Calling eq on Anys.
    def ref_==(y: Any): Boolean = x.toRef eq y.toRef

    // Short decoded class name.
    def shortClass: String = decodeName(x.getClass.getName split "[.]" last)

    def maybe[B](pf: A ?=> B): Option[B] = pf lift x
    def try_s[A1 >: A](implicit shows: Show[A1] = null): String = if (shows == null) any_s else (x: A1).to_s
    def any_s: String = x match {
      case x: ShowDirect => x.to_s
      case _             => "" + x
    }
    // Dribbling implicit parameters onto extension methods to preserve AnyVal-extension methods.
    def ===(that: A)(implicit eq: Eq[A]): Boolean = eq.equiv(x, that)
    def !==(that: A)(implicit eq: Eq[A]): Boolean = !eq.equiv(x, that)
  }

  final class OptionOps[A](private val x: Option[A]) extends AnyVal {
    def or(alt: => A): A         = x getOrElse alt
    def | (alt: => A): A         = x getOrElse alt
    def ||(alt: => A): Option[A] = x orElse Some(alt)
  }

  final class TryOps[A](private val x: Try[A]) extends AnyVal {
    def | (expr: => A): A = x.toOption getOrElse expr
    def ||(expr: => A): Try[A] = x match {
      case scala.util.Failure(_) => Try(expr)
      case _                     => x
    }
    def fold[B](f: A => B, g: Throwable => B): B = x match {
      case scala.util.Success(x) => f(x)
      case scala.util.Failure(t) => g(t)
    }
  }
  final class IntOps(private val self: Int) extends AnyVal {
    private type This = Int

    def cmp: Order.Cmp        = Order.Cmp(self)
    def abs: This             = math.abs(self)
    def max(that: This): This = math.max(self, that)
    def min(that: This): This = math.min(self, that)
    def signum: This          = math.signum(self)

    def u: UInt        = UInt(self)
    def binary: String = jl.Integer.toBinaryString(self)
    def hex: String    = jl.Integer.toHexString(self)
    def octal: String  = jl.Integer.toOctalString(self)
  }
  final class LongOps(private val self: Long) extends AnyVal {
    private type This = Long

    def cmp: Order.Cmp        = Order.Cmp(self)
    def abs: This             = math.abs(self)
    def max(that: This): This = math.max(self, that)
    def min(that: This): This = math.min(self, that)
    def signum: This          = math.signum(self)

    def toUnsignedInt: UInt = UInt(self)
    def binary: String      = jl.Long.toBinaryString(self)
    def hex: String         = jl.Long.toHexString(self)
    def octal: String       = jl.Long.toOctalString(self)
  }
  final class BooleanAlgebraOps[A](private val algebra: BooleanAlgebra[A]) extends AnyVal {
    def map[B](f: B => A, g: A => B): BooleanAlgebra[B] = new BooleanAlgebra.MappedAlgebra[A, B](algebra, f, g)
  }
  final class Function1Ops[-T, +R](private val f: T => R) extends AnyVal {
    def labeled(label: String): T => R = new LabeledFunction(f, label)
  }
}


final class PartialFunctionOps[T, R](private val pf: T ?=> R) extends AnyVal {
  def labeled(label: String): T ?=> R     = new LabeledPartialFunction(pf, label)
  def contraMap[T1](f: T1 => T): T1 ?=> R = { case x if pf isDefinedAt f(x) => pf(f(x)) }
  def mapValues[R1](f: R => R1): T ?=> R1 = { case x if pf isDefinedAt x => f(pf(x)) }
}
final class LabeledFunction[-T, +R](f: T => R, val label: String) extends (T => R) with Labeled {
  def apply(x: T): R = f(x)
}
final class LabeledPartialFunction[-T, +R](pf: PartialFunction[T, R], val label: String) extends PartialFunction[T, R] with Labeled {
  def isDefinedAt(x: T) = pf isDefinedAt x
  def apply(x: T): R    = pf(x)
}
final class ScalaIterator[A](xs: jIterator[A]) extends scala.Iterator[A] {
  def next    = xs.next
  def hasNext = xs.hasNext
}
