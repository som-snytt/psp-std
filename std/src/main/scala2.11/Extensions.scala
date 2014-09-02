package psp
package std

import java.{ lang => jl }
import scala.{ collection => sc }
import Trilean._
import psp.std.api.Cmp._

object TClass {
  final class OrderOps[A](private val lhs: A) extends AnyVal {
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
    private def evaluate(rhs: A)(trues: PCmp*)(undefs: PCmp*)(implicit ord: PartialOrder[A]): Trilean = {
      val res = partialCompare(rhs)
      if (trues contains res) True
      else if (undefs contains res) Undefined
      else False
    }
    def partialCompare(rhs: A)(implicit ord: PartialOrder[A]): PCmp = ord.partialCompare(lhs, rhs)
    def < (rhs: A)(implicit ord: PartialOrder[A]): Trilean          = evaluate(rhs)(LT)(NA, LE)
    def <=(rhs: A)(implicit ord: PartialOrder[A]): Trilean          = evaluate(rhs)(LT, LE, EQ)(NA)
    def >=(rhs: A)(implicit ord: PartialOrder[A]): Trilean          = evaluate(rhs)(GT, GE, EQ)(NA)
    def >(rhs: A)(implicit ord: PartialOrder[A]): Trilean           = evaluate(rhs)(GT)(NA, GE)
    def min(rhs: A)(implicit ord: PartialOrder[A]): Option[A]       = partialCompare(rhs) match { case LT | LE => Some(lhs) ; case GT | GE => Some(rhs) ; case _ => None }
    def max(rhs: A)(implicit ord: PartialOrder[A]): Option[A]       = partialCompare(rhs) match { case LT | LE => Some(rhs) ; case GT | GE => Some(lhs) ; case _ => None }
  }
  final class AlgebraOps[A](private val lhs: A) extends AnyVal {
    def implies(rhs: A)(implicit alg: BooleanAlgebra[A]) = !lhs || rhs
    def && (rhs: A)(implicit alg: BooleanAlgebra[A]): A  = if (isOne) rhs else if (rhs.isOne) lhs else if (lhs.isZero || rhs.isZero) alg.zero else alg.and(lhs, rhs)
    def || (rhs: A)(implicit alg: BooleanAlgebra[A]): A  = if (isZero) rhs else if (rhs.isZero) lhs else if (lhs.isOne || rhs.isOne) alg.one else alg.or(lhs, rhs)
    def unary_!(implicit alg: BooleanAlgebra[A]): A      = if (isZero) alg.one else if (isOne) alg.zero else alg.not(lhs)
    def isZero(implicit alg: BooleanAlgebra[A]): Boolean = alg.zero ref_== lhs
    def isOne(implicit alg: BooleanAlgebra[A]): Boolean  = alg.one ref_== lhs
  }
  final class EqOps[A](private val lhs: A) extends AnyVal {
    def ===(rhs: A)(implicit eq: Eq[A]): Boolean = eq.equiv(lhs, rhs)
    def !==(rhs: A)(implicit eq: Eq[A]): Boolean = !eq.equiv(lhs, rhs)
  }
  final class HashEqOps[A](private val lhs: A) extends AnyVal {
    def hash(implicit eq: HashEq[A]): Int = eq hash lhs
  }
}

object Ops {
  final val InputStreamBufferSize = 8192

  // Have to each go into their own class because the apply methods have the same erasure.
  final class Seq1[A](private val xs: sc.Seq[A]) extends AnyVal {
    def apply(nth: Nth): A = if (nth.isUndefined) sys.error(s"apply($nth)") else xs(nth.intIndex)
  }
  final class Seq2[A](private val xs: sc.Seq[A]) extends AnyVal {
    def apply(index: Index): A = if (index.isUndefined) sys.error(s"apply($index)") else xs(index.value)
  }
  final class SeqOps[A](private val xs: sc.Seq[A]) extends AnyVal with SeqLikeOps[A] {
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
    def sortedKeys(implicit ord: Order[K])                     = map.keys.toSeq.sorted(ord.toOrdering)
    def orderByKey(implicit ord: Order[K]): OrderedMap[K, V]   = orderedMap(sortedKeys, map.toMap)
    def orderByValue(implicit ord: Order[V]): OrderedMap[K, V] = orderedMap(sortedKeys(ord on map), map.toMap)
  }
  final class SortedMap[K, V](private val map: sc.SortedMap[K, V]) extends AnyVal {
    private def ord: Order[K] = Order fromOrdering map.ordering
    def reverse: OrderedMap[K, V] = map orderByKey ord.reverse
  }
  final class GTOnceOps[A](private val xs: GTOnce[A]) extends AnyVal with FoldableOps[A] {
    def foldl[B](zero: B)(f: (B, A) => B): B                       = xs.foldLeft(zero)(f)
    def findOr(p: A => Boolean, alt: => A): A                      = (xs find p) | alt
    def sortOrder[B: Order](f: A => B): Vector[A]                  = xs.toVector sorted (?[Order[B]] on f toOrdering)
    def sortDistinct(implicit ord: Order[A]): Vector[A]            = distinct.sorted(ord.toOrdering)
    def mapZip[B, C](ys: GTOnce[B])(f: (A, B) => C): Vector[C]     = for ((x, y) <- xs.toVector zip ys.toVector) yield f(x, y)
    def mapFrom[B](f: A => B): OrderedMap[B, A]                    = orderedMap(xs.toVector map (x => (f(x), x)): _*)
    def mapOnto[B](f: A => B): OrderedMap[A, B]                    = orderedMap(xs.toVector map (x => (x, f(x))): _*)
    def mapToAndOnto[B, C](k: A => B, v: A => C): OrderedMap[B, C] = xs.toVector |> (xs => orderedMap(xs map (x => k(x) -> v(x)): _*))
    def mapToMapPairs[B, C](f: A => (B, C)): OrderedMap[B, C]      = xs.toVector |> (xs => orderedMap(xs map f: _*))
    def sorted(implicit ord: Order[A]): Vector[A]                  = xs.toVector.sorted(ord.toOrdering)
    def distinct: Vector[A]                                        = xs.toVector.distinct
    def unsortedFrequencyMap: Map[A, Int]                          = immutableMap(xs.toVector groupBy (x => x) mapValues (_.size) toSeq: _*)
    def ascendingFrequency: OrderedMap[A, Int]                     = unsortedFrequencyMap |> (_.orderByValue)
    def descendingFrequency: OrderedMap[A, Int]                    = ascendingFrequency.reverse
  }
  final class ArrayOps[A](private val xs: Array[A]) extends AnyVal with SeqLikeOps[A] {
    def length                                   = xs.length
    def index(elem: A): Index                    = indexRange find (i => xs(i.value) == elem)
    def lastIndex(elem: A): Index                = indexRange findReverse (i => xs(i.value) == elem)
    def indexAtWhich(p: A => Boolean): Index     = indexRange find (i => p(xs(i.value)))
    def lastIndexAtWhich(p: A => Boolean): Index = indexRange findReverse (i => p(xs(i.value)))
    def hasElem(elem: A): Boolean                = indexRange exists (i => xs(i.value) == elem)

    def apply(range: IndexRange)(implicit tag: ClassTag[A]): Array[A] = xs.slice(indexRange intersect range).force
    def toSeq: ISeq[A] = immutableSeq(xs: _*)
  }
  final class AnyOps[A](private val x: A) extends AnyVal {
    // Short decoded class name.
    def shortClass: String = decodeName(x.getClass.getName split "[.]" last)
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

    def cmp: Cmp              = Order difference self
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

    def compared(that: Long): Cmp = Order difference (self - that)
    def cmp: Cmp                  = Order difference self
    def abs: This                 = math.abs(self)
    def max(that: This): This     = math.max(self, that)
    def min(that: This): This     = math.min(self, that)
    def signum: This              = math.signum(self)

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
  final class InputStreamOps(private val in: InputStream) extends AnyVal {
    private def wrap[A] (f: InputStream => A): A = {
      val in = this.buffered()
      try f(in) finally in.close()
    }
    def slurp(): Array[Byte] = slurp(-1)
    def slurp(len: Int): Array[Byte] = {
      val buf = Array.newBuilder[Byte]
      if (len >= 0) buf sizeHint len
      wrap { in =>
        var offset = 0
        val arr = new Array[Byte](InputStreamBufferSize)
        def loop() {
          if (offset < len || len < 0) {
            val read = in.read(arr, 0, InputStreamBufferSize)
            if (read >= 0) {
              offset += read
              buf ++= (arr take read).toSeq
              loop()
            }
          }
        }
        loop()
        buf.result doto (xs => assert(len < 0 || xs.length == len, s"Could not read entire source ($offset of $len bytes)"))
      }
    }
    def buffered(): BufferedInputStream = in match {
      case buf: BufferedInputStream => buf
      case _                        => new BufferedInputStream(in)
    }
  }
  final class IteratorOps[A](it: jIterator[A]) {
    def toScalaIterator: sIterator[A] = new ScalaIterator(it)
    def toForeach: Foreach[A]         = each(toScalaIterator)
  }
  final class jCollectionOps[A](private val xs: jAbstractCollection[A]) extends AnyVal {
    def toForeach: Foreach[A]         = each(toTraversable)
    def toTraversable: Traversable[A] = toScalaIterator.toTraversable
    def toScalaIterator: sIterator[A] = new ScalaIterator(xs.iterator)
  }

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

  trait FoldableOps[A] extends Any {
    def foldl[B](zero: B)(f: (B, A) => B): B

    private def stringed(sep: String)(f: A => String): String =
      foldl(new StringBuilder)((sb, x) => if (sb.isEmpty) sb append f(x) else sb append sep append f(x) ).result

    def join(sep: String)(implicit shows: Show[A]): String = stringed(sep)(_.to_s)
    def joinLines(implicit shows: Show[A]): String         = join(EOL)
    def joinComma(implicit shows: Show[A]): String         = join(", ")
    def joinSpace(implicit shows: Show[A]): String         = join(" ")
    def mkString(sep: String): String                      = stringed(sep)(_.try_s)
    def find(p: Predicate[A]): Option[A]                   = foldl[Option[A]](None)((res, x) => if (p(x)) return Some(x) else res)
    def forall(p: Predicate[A]): Boolean                   = foldl[Boolean](true)((res, x) => if (!p(x)) return false else res)
    def exists(p: Predicate[A]): Boolean                   = foldl[Boolean](false)((res, x) => if (p(x)) return true else res)
  }

  trait ConversionOps[A] extends Any {
    def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A]
    private def toScala[CC[X]](implicit z: CanBuild[A, CC[A]]): CC[A] = to[CC](Builds wrap z)

    def toSet(implicit z: HashEq[A]): EquivSet[A]  = to[EquivSet]
    def toIndexed: Direct[A]                       = to[Direct]
    def toPspList: PspList[A]                      = to[PspList]
    def toArray(implicit z: ClassTag[A]): Array[A] = toScala[Array]

    def toIterable: Iterable[A]       = toScala[Iterable]
    def toList: List[A]               = toScala[List]
    def toScalaSet: Set[A]            = toScala[Set]
    def toSeq: ISeq[A]                = toScala[ISeq]
    def toStream: Stream[A]           = toScala[Stream]
    def toTraversable: Traversable[A] = toScala[Traversable]
    def toVector: Vector[A]           = toScala[Vector]

    def trav: Traversable[A]       = toTraversable
    def scalaIterator: Iterator[A] = toIterable.iterator
  }

  final class IndexedSeqOps[A](private val xs: Direct[A]) extends AnyVal with FoldableOps[A] with ConversionOps[A] {
    def toRepr[Repr](implicit z: Builds[A, Repr]): Repr = z build xs
    def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A]  = z build xs

    def foldl[B](zero: B)(f: (B, A) => B): B = {
      var result = zero
      xs.foreach(x => result = f(result, x))
      result
    }
  }

  final class ForeachOps[A](private val xs: Foreach[A]) extends AnyVal with FoldableOps[A] with ConversionOps[A] {
    def toRepr[Repr](implicit z: Builds[A, Repr]): Repr = z build xs
    def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A]  = z build xs

    def sum(implicit num: Numeric[A]): A     = foldl(num.zero)(num.plus)
    def product(implicit num: Numeric[A]): A = foldl(num.one)(num.times)

    def foldl[B](zero: B)(f: (B, A) => B): B = {
      var result = zero
      xs.foreach(x => result = f(result, x))
      result
    }
    def foldr[B](zero: B)(f: (A, B) => B): B          = {
      var result = zero
      xs.foreach(x => result = f(x, result))
      result
    }
    def toDirect: Direct[A] = xs match {
      case xs: Direct[_] => xs
      case _             => Direct elems (toSeq: _*)
    }
  }

  /** Hand specialized on the left, @specialized on the right, value classes for tuple creation.
   */
  final class ArrowAssocInt(private val self: Int) extends AnyVal {
    @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Int, B] = Tuple2(self, y)
  }
  final class ArrowAssocLong(private val self: Long) extends AnyVal {
    @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Long, B] = Tuple2(self, y)
  }
  final class ArrowAssocDouble(private val self: Double) extends AnyVal {
    @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Double, B] = Tuple2(self, y)
  }
  final class ArrowAssocChar(private val self: Char) extends AnyVal {
    @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Char, B] = Tuple2(self, y)
  }
  final class ArrowAssocBoolean(private val self: Boolean) extends AnyVal {
    @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Boolean, B] = Tuple2(self, y)
  }
  final class ArrowAssocRef[A](private val self: A) extends AnyVal {
    @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(self, y)
  }

  final class SizeInfoOps(val lhs: SizeInfo) extends AnyVal {
    import PartialOrder._, SizeInfo._

    def isZero    = lhs == Precise(Zero)
    def isFinite  = lhs.hiBound != Infinite
    def isPrecise = lhs match { case _: Precise => true ; case _ => false }

    def atMost: SizeInfo  = bounded(Zero, lhs)
    def atLeast: SizeInfo = bounded(lhs, Infinite)
    def hiBound: Atomic = lhs match {
      case Bounded(_, hi) => hi
      case x: Atomic      => x
    }

    def slice(range: IndexRange): SizeInfo = (lhs - range.start.toSize) min range.size

    def precisely: Option[Int] = lhs match { case Precise(Size(n)) => Some(n) ; case _ => None }
    def preciseOr(alt: => Int): Int = precisely getOrElse alt
    def preciseIntSize: Int = preciseOr(sys error s"precise size unavailable: $lhs")

    def * (rhs: Size): SizeInfo = lhs match {
      case Precise(n)               => precise(n.value * rhs.value)
      case Bounded(lo, Precise(hi)) => bounded(Size(lo.value * rhs.value), precise(hi.value * rhs.value))
      case Bounded(lo, _)           => if (rhs.isZero) Unknown else Bounded(lo * rhs.value, Infinite)
      case Infinite                 => if (rhs.isZero) Unknown else Infinite
    }

    def + (rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, _) | (_, Infinite)            => Infinite
      case (Precise(l), Precise(r))                 => Precise(Size(l.value + r.value))
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(Size(l1.value + l2.value), h1 + h2)
    }
    def - (rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, Finite(_, _))         => Infinite
      case (Finite(_, _), Infinite)         => Empty
      case (Finite(l1, h1), Finite(l2, h2)) => bounded(l1 - h2, Precise(h1 - l2))
      case (Bounded(l1, h1), Precise(n))    => bounded(l1 - n, h1 - Precise(n))
      case _                                => Unknown
    }
    def min(rhs: SizeInfo): SizeInfo = lhs partialCompare rhs match {
      case LT | LE | EQ => lhs
      case GT | GE      => rhs
      case _            => onBounds(rhs)((l1, h1, l2, h2) => bounded(l1 min l2, h1 min h2))
    }
    def max(rhs: SizeInfo): SizeInfo = lhs partialCompare rhs match {
      case LT | LE | EQ => rhs
      case GT | GE      => lhs
      case _            => onBounds(rhs)((l1, h1, l2, h2) => bounded(l1 max l2, h1 max h2))
    }

    private def onBounds[T](rhs: SizeInfo)(f: (Size, Atomic, Size, Atomic) => T): T = {
      val GenBounded(l1, h1) = lhs
      val GenBounded(l2, h2) = rhs

      f(l1, h1, l2, h2)
    }
  }
}

final class LabeledFunction[-T, +R](f: T => R, val label: String) extends (T => R) with api.Labeled {
  def apply(x: T): R = f(x)
}
final class LabeledPartialFunction[-T, +R](pf: PartialFunction[T, R], val label: String) extends PartialFunction[T, R] with api.Labeled {
  def isDefinedAt(x: T) = pf isDefinedAt x
  def apply(x: T): R    = pf(x)
}
