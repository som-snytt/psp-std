package psp
package std

import java.{ lang => jl }
import api._

object TClass {
  final class OrderOps[A](val __order_lhs: A) extends AnyVal {
    import Cmp._
    def compare(rhs: A)(implicit ord: Order[A]): Cmp = ord.compare(__order_lhs, rhs)

    def < (rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) == LT
    def <=(rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) != GT
    def > (rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) == GT
    def >=(rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) != LT
    def min(rhs: A)(implicit ord: Order[A]): A       = if (this < rhs) __order_lhs else rhs
    def max(rhs: A)(implicit ord: Order[A]): A       = if (this > rhs) __order_lhs else rhs
  }
  final class PartialOrderOps[A](val __porder_lhs: A) extends AnyVal {
    def partialCompare(rhs: A)(implicit ord: PartialOrder[A]): PCmp    = ord.partialCompare(__porder_lhs, rhs)
    def tryCompare(rhs: A)(implicit ord: PartialOrder[A]): Option[Cmp] = partialCompare(rhs) match {
      case PCmp.LT => Some(Cmp.LT)
      case PCmp.GT => Some(Cmp.GT)
      case PCmp.EQ => Some(Cmp.EQ)
      case _       => None
    }

    def p_< (rhs: A)(implicit ord: PartialOrder[A]): Boolean = partialCompare(rhs) == PCmp.LT
    def p_<=(rhs: A)(implicit ord: PartialOrder[A]): Boolean = tryCompare(rhs) exists (_ != Cmp.GT)
    def p_> (rhs: A)(implicit ord: PartialOrder[A]): Boolean = partialCompare(rhs) == PCmp.GT
    def p_>=(rhs: A)(implicit ord: PartialOrder[A]): Boolean = tryCompare(rhs) exists (_ != Cmp.LT)

    def pmin(rhs: A)(implicit ord: PartialOrder[A]): Option[A] = tryCompare(rhs) map { case Cmp.GT => rhs ; case _ => __porder_lhs }
    def pmax(rhs: A)(implicit ord: PartialOrder[A]): Option[A] = tryCompare(rhs) map { case Cmp.LT => rhs ; case _ => __porder_lhs }
  }
  final class AlgebraOps[A](val lhs: A) extends AnyVal {
    def implies(rhs: A)(implicit alg: BooleanAlgebra[A]) = !lhs || rhs
    def && (rhs: A)(implicit alg: BooleanAlgebra[A]): A  = if (isOne) rhs else if (rhs.isOne) lhs else if (lhs.isZero || rhs.isZero) alg.zero else alg.and(lhs, rhs)
    def || (rhs: A)(implicit alg: BooleanAlgebra[A]): A  = if (isZero) rhs else if (rhs.isZero) lhs else if (lhs.isOne || rhs.isOne) alg.one else alg.or(lhs, rhs)
    def unary_!(implicit alg: BooleanAlgebra[A]): A      = if (isZero) alg.one else if (isOne) alg.zero else alg.not(lhs)
    def isZero(implicit alg: BooleanAlgebra[A]): Boolean = alg.zero ref_== lhs
    def isOne(implicit alg: BooleanAlgebra[A]): Boolean  = alg.one ref_== lhs
  }
  final class EqOps[A](val lhs: A) extends AnyVal {
    def ===(rhs: A)(implicit eq: Eq[A]): Boolean = eq.equiv(lhs, rhs)
    def !==(rhs: A)(implicit eq: Eq[A]): Boolean = !eq.equiv(lhs, rhs)
  }
  final class HashEqOps[A](val lhs: A) extends AnyVal {
    def hash(implicit eq: HashEq[A]): Int = eq hash lhs
  }
}

object Ops {
  final val InputStreamBufferSize = 8192

  // Have to each go into their own class because the apply methods have the same erasure.
  final class Seq1[A](val xs: scSeq[A]) extends AnyVal {
    def apply(nth: Nth): A = if (nth.isUndefined) sys.error(s"apply($nth)") else xs(nth.intIndex)
  }
  final class Seq2[A](val xs: scSeq[A]) extends AnyVal {
    def apply(index: Index): A = if (index.isUndefined) sys.error(s"apply($index)") else xs(index.value)
  }
  final class SeqOps[A](val xs: scSeq[A]) extends AnyVal with SeqLikeOps[A] {
    def toRefs: scSeq[AnyRef] = xs map (_.toRef)
    def elemAt(index: Index): A = if (index.isUndefined) sys.error(s"apply($index)") else xs(index.value)

    def length                                   = xs.length
    def index(elem: A): Index                    = Index(xs indexOf elem)
    def lastIndex(elem: A): Index                = Index(xs lastIndexOf elem)
    def indexAtWhich(p: A => Boolean): Index     = Index(xs indexWhere p)
    def lastIndexAtWhich(p: A => Boolean): Index = Index(xs lastIndexWhere p)
    def hasElem(elem: A): Boolean                = xs contains elem
    // Produces a vector containing the elements in the given index range.
    // Ignores indices which don't exist in the target sequence.
    def apply(range: IndexRange): Vector[A] = indexRange intersect range map elemAt
  }
  final class Map[K, V](val map: scMap[K, V]) extends AnyVal {
    def sortedKeys(implicit ord: Order[K])                     = map.keys.toSeq.sorted(ordering[K])
    def orderByKey(implicit ord: Order[K]): OrderedMap[K, V]   = orderedMap(sortedKeys, map.toMap)
    def orderByValue(implicit ord: Order[V]): OrderedMap[K, V] = orderedMap(sortedKeys(ord on map), map.toMap)
  }
  final class SortedMap[K, V](val map: scala.collection.SortedMap[K, V]) extends AnyVal {
    private def ord: Order[K]     = Order create map.ordering
    def reverse: OrderedMap[K, V] = map orderByKey ord.reverse
  }

  final class GTOnceOps[A](val xs: GTOnce[A]) extends AnyVal with FoldableOps[A] {
    def ascendingFrequency: OrderedMap[A, Int]                     = unsortedFrequencyMap |> (_.orderByValue)
    def descendingFrequency: OrderedMap[A, Int]                    = ascendingFrequency.reverse
    def distinct: Vector[A]                                        = xs.toVector.distinct
    def findOr(p: A => Boolean, alt: => A): A                      = (xs find p) | alt
    def foldl[B](zero: B)(f: (B, A) => B): B                       = xs.foldLeft(zero)(f)
    def mapFrom[B](f: A => B): OrderedMap[B, A]                    = orderedMap(xs.toVector map (x => (f(x), x)): _*)
    def mapOnto[B](f: A => B): OrderedMap[A, B]                    = orderedMap(xs.toVector map (x => (x, f(x))): _*)
    def mapToAndOnto[B, C](k: A => B, v: A => C): OrderedMap[B, C] = xs.toVector |> (xs => orderedMap(xs map (x => k(x) -> v(x)): _*))
    def mapToMapPairs[B, C](f: A => (B, C)): OrderedMap[B, C]      = xs.toVector |> (xs => orderedMap(xs map f: _*))
    def mapZip[B, C](ys: GTOnce[B])(f: (A, B) => C): Vector[C]     = for ((x, y) <- xs.toVector zip ys.toVector) yield f(x, y)
    def sortDistinct(implicit ord: Order[A]): Vector[A]            = distinct sorted ord.toScalaOrdering
    def sortOrder[B: Order](f: A => B): Vector[A]                  = xs.toVector sorted (Order.order[B] on f).toScalaOrdering
    def sorted(implicit ord: Order[A]): Vector[A]                  = xs.toVector sorted Order.order[A].toScalaOrdering
    def unsortedFrequencyMap: Map[A, Int]                          = sciMap(xs.toVector groupBy identity mapValues (_.size) toSeq: _*)
    def sameMembers(ys: GTOnce[A])(implicit eqs: HashEq[A])        = EquivSet[A](each(xs)) == EquivSet[A](each(ys))
  }

  final class ArrayOps[A](val xs: Array[A]) extends AnyVal with SeqLikeOps[A] with FoldableOps[A] {
    def foldl[B](zero: B)(f: (B, A) => B): B = {
      var result = zero
      indexRange foreach (i => result = f(result, elemAt(i)))
      result
    }
    def elemAt(index: Index): A                  = if (index.isUndefined) sys.error(s"apply($index)") else xs(index.value)
    def length                                   = xs.length
    def index(elem: A): Index                    = indexRange find (i => elemAt(i) == elem)
    def lastIndex(elem: A): Index                = indexRange findReverse (i => elemAt(i) == elem)
    def indexAtWhich(p: A => Boolean): Index     = indexRange find (i => p(elemAt(i)))
    def lastIndexAtWhich(p: A => Boolean): Index = indexRange findReverse (i => p(elemAt(i)))
    def hasElem(elem: A): Boolean                = indexRange exists (i => elemAt(i) == elem)

    def slice(start: Int, end: Int)(implicit tag: ClassTag[A]): Array[A] = apply(indexRange drop start take (end - start)) //IndexRange.until(start, end))
    def take(n: Int)(implicit tag: ClassTag[A]): Array[A]                = if (n <= 0) Array() else if (n >= xs.length) xs else apply(psp.std.indexRange(0, n))
    def drop(n: Int)(implicit tag: ClassTag[A]): Array[A]                = if (n <= 0) xs else if (n >= xs.length) Array() else apply(psp.std.indexRange(n, length))
    def dropWhile(p: A => Boolean)(implicit tag: ClassTag[A]): Array[A]  = apply(indexRange dropWhile (i => p(elemAt(i))))
    def takeWhile(p: A => Boolean)(implicit tag: ClassTag[A]): Array[A]  = apply(indexRange takeWhile (i => p(elemAt(i))))

    def apply(range: IndexRange)(implicit tag: ClassTag[A]): Array[A] = xs.m.slice(indexRange intersect range).native
    def toSeq: sciSeq[A] = sciSeq(xs: _*)
  }
  final class AnyOps[A](val x: A) extends AnyVal {
    // Short decoded class name.
    def shortClass: String = decodeName(x.getClass.getName.dottedSegments.last)

    // "Maybe we can enforce good programming practice with annoyingly long method names."
    def castTo[U] : U   = x.asInstanceOf[U]
    def toRef: AnyRef   = castTo[AnyRef]
    def isNull: Boolean = toRef eq null

    def reflect[B](m: jMethod)(args: Any*): B = m.invoke(x, args.toRefs: _*).castTo[B]

    // The famed forward pipe.
    @inline def |>[B](f: A => B): B       = f(x)
    @inline def doto(f: A => Unit): A     = sideEffect(f(x))
    @inline def sideEffect(body: Unit): A = x

    // Calling eq on Anys.
    def ref_==(y: Any): Boolean = toRef eq y.toRef
    def id_## : Int             = System identityHashCode x

    def maybe[B](pf: PartialFunction[A, B]): Option[B] = pf lift x
    def try_s[A1 >: A](implicit shows: Show[A1] = null): String = if (shows == null) any_s else x.to_s
    def any_s: String = x match {
      case x: ShowDirect => x.to_s
      case _             => "" + x
    }
  }
  final class IntOps(val self: Int) extends AnyVal {
    private type This = Int

    def cmp: Cmp              = Order difference self
    def abs: This             = math.abs(self)
    def max(that: This): This = math.max(self, that)
    def min(that: This): This = math.min(self, that)
    def signum: This          = math.signum(self)

    def until(end: Int): scala.Range = scala.Range(self, end) // XXX
    def to(end: Int): scala.Range    = scala.Range.inclusive(self, end) // XXX

    def u: UInt        = UInt(self)
    def binary: String = jl.Integer.toBinaryString(self)
    def hex: String    = jl.Integer.toHexString(self)
    def octal: String  = jl.Integer.toOctalString(self)
  }
  final class LongOps(val self: Long) extends AnyVal {
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
  final class BooleanAlgebraOps[A](val algebra: BooleanAlgebra[A]) extends AnyVal {
    def map[B](f: B => A, g: A => B): BooleanAlgebra[B] = new Algebras.Mapped[A, B](algebra, f, g)
  }
  final class Function1Ops[T, R](val f: T => R) extends AnyVal {
    def labeled(label: String): T => R                       = new LabeledFunction(f, label)
    def sameAt(g: T => R)(implicit eqs: Eq[R]): Predicate[T] = x => f(x) === g(x)
  }
  final class InputStreamOps(val in: InputStream) extends AnyVal {
    private def wrap[A](f: InputStream => A): A = buffered |> (in => f(in) sideEffect in.close)

    //   val in = this.buffered()
    //   try f(in) finally in.close()
    // }
    def slurp(): Array[Byte] = slurp(-1)
    def slurp(len: Int): Array[Byte] = {
      val buf = arrayBuilder[Byte]()
      if (len >= 0) buf sizeHint len
      wrap { in =>
        var offset = 0
        val arr = new Array[Byte](InputStreamBufferSize)
        def loop() {
          if (offset < len || len < 0) {
            val read = in.read(arr, 0, InputStreamBufferSize)
            if (read >= 0) {
              offset += read
              buf ++= (arr.m take read).toSeq
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
    def toScalaIterator: scIterator[A] = new ScalaIterator(it)
    def toForeach: Foreach[A]          = each(toScalaIterator)
  }
  final class jCollectionOps[A](val xs: jAbstractCollection[A]) extends AnyVal {
    def toForeach: Foreach[A]          = each(toTraversable)
    def toTraversable: Traversable[A]  = toScalaIterator.toTraversable
    def toScalaIterator: scIterator[A] = new ScalaIterator(xs.iterator)
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
    def elemAt(index: Index): A

    def indicesAtWhich(p: A => Boolean): Vector[Index] = indexRange.toVector filter (i => p(elemAt(i)))
    def hasIndex(index: Index): Boolean                = indexRange contains index
    def indexRange: IndexRange                         = exclusiveEnd.indicesUntil
    def exclusiveEnd: Index                            = Index(length)
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
    def toPolicyList: PolicyList[A]                = to[PolicyList]
    def toArray(implicit z: ClassTag[A]): Array[A] = toScala[Array]

    def toIterable: Iterable[A]       = toScala[Iterable]
    def toList: List[A]               = toScala[List]
    def toScalaSet: Set[A]            = toScala[Set]
    def toSeq: sciSeq[A]              = toScala[sciSeq]
    def toStream: Stream[A]           = toScala[Stream]
    def toTraversable: Traversable[A] = toScala[Traversable]
    def toVector: Vector[A]           = toScala[Vector]

    def trav: Traversable[A]       = toTraversable
    def scalaIterator: Iterator[A] = toIterable.iterator
  }

  final class IndexedSeqOps[A](val __psp_xs1: Direct[A]) extends AnyVal with FoldableOps[A] with ConversionOps[A] {
    def toRepr[Repr](implicit z: Builds[A, Repr]): Repr = z build __psp_xs1
    def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A]  = z build __psp_xs1

    def foldl[B](zero: B)(f: (B, A) => B): B = {
      var result = zero
      __psp_xs1.foreach(x => result = f(result, x))
      result
    }
  }

  final class ForeachOps[A](val __psp_xs2: Foreach[A]) extends AnyVal with FoldableOps[A] with ConversionOps[A] {
    def toRepr[Repr](implicit z: Builds[A, Repr]): Repr = z build __psp_xs2
    def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A]  = z build __psp_xs2

    def sum(implicit num: Numeric[A]): A     = foldl(num.zero)(num.plus)
    def product(implicit num: Numeric[A]): A = foldl(num.one)(num.times)

    def foldl[B](zero: B)(f: (B, A) => B): B = {
      var result = zero
      __psp_xs2.foreach(x => result = f(result, x))
      result
    }
    def foldr[B](zero: B)(f: (A, B) => B): B          = {
      var result = zero
      __psp_xs2.foreach(x => result = f(x, result))
      result
    }
    def toDirect: Direct[A] = __psp_xs2 match {
      case xs: Direct[_] => xs
      case _             => Direct elems (toSeq: _*)
    }
  }

  final class OptionOps[A](val __psp_x: Option[A]) extends AnyVal {
    def or(alt: => A): A         = __psp_x getOrElse alt
    def | (alt: => A): A         = __psp_x getOrElse alt
    def ||(alt: => A): Option[A] = __psp_x orElse Some(alt)
  }
  final class TryOps[A](val __psp_x: scala.util.Try[A]) extends AnyVal {
    def | (expr: => A): A = __psp_x match {
      case scala.util.Failure(_) => expr
      case scala.util.Success(x) => x
    }
    def ||(expr: => A): scala.util.Try[A] = __psp_x match {
      case x @ scala.util.Success(_) => x
      case scala.util.Failure(_)     => scala.util.Try(expr)
    }
    def fold[B](f: A => B, g: Throwable => B): B = __psp_x match {
      case scala.util.Success(x) => f(x)
      case scala.util.Failure(t) => g(t)
    }
  }

  final class CharOps(val ch: Char) extends AnyVal {
    def isDigit      = Character isDigit ch
    def toUpper      = Character toUpperCase ch
    def toLower      = Character toLowerCase ch
    def isUpper      = Character isUpperCase ch
    def isLower      = Character isLowerCase ch
    def isWhitespace = Character isWhitespace ch
    def isControl    = Character isISOControl ch
  }

  /** Hand specialized on the left, @specialized on the right, value classes for tuple creation.
   */
   final class ArrowAssocInt(val self: Int) extends AnyVal {
    @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Int, B] = Tuple2(self, y)
  }
  final class ArrowAssocLong(val self: Long) extends AnyVal {
    @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Long, B] = Tuple2(self, y)
  }
  final class ArrowAssocDouble(val self: Double) extends AnyVal {
    @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Double, B] = Tuple2(self, y)
  }
  final class ArrowAssocChar(val self: Char) extends AnyVal {
    @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Char, B] = Tuple2(self, y)
  }
  final class ArrowAssocBoolean(val self: Boolean) extends AnyVal {
    @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Boolean, B] = Tuple2(self, y)
  }
  final class ArrowAssocRef[A](val self: A) extends AnyVal {
    @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(self, y)
  }

  final class SizeInfoOps(val lhs: SizeInfo) extends AnyVal {
    import SizeInfo._

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

    def + (rhs: Size): SizeInfo = this + Precise(rhs)
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

    def min(rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, _)                            => rhs
      case (_, Infinite)                            => lhs
      case (Precise(x), Precise(y))                 => if (x <= y) lhs else rhs
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 min l2, h1 min h2)
    }
    def max(rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, _)                            => Infinite
      case (_, Infinite)                            => Infinite
      case (Precise(x), Precise(y))                 => if (x >= y) lhs else rhs
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 max l2, h1 max h2)
    }
  }

  final class OrderBy[A] { def apply[B](f: A => B)(implicit ord: Order[B]): Order[A] = Order[A]((x, y) => ord.compare(f(x), f(y))) }
  final class EqBy[A]    { def apply[B](f: A => B)(implicit equiv: Eq[B]): Eq[A]     = Eq[A]((x, y) => equiv.equiv(f(x), f(y))) }
  final class ShowBy[A]  { def apply[B](f: A => B)(implicit show: Show[B]): Show[A]  = Show[A](x => show show f(x)) }
}

final class LabeledFunction[-T, +R](f: T => R, val to_s: String) extends (T => R) with ShowDirectNow {
  def apply(x: T): R = f(x)
}
final class LabeledPartialFunction[-T, +R](pf: PartialFunction[T, R], val to_s: String) extends PartialFunction[T, R] with ShowDirectNow {
  def isDefinedAt(x: T) = pf isDefinedAt x
  def apply(x: T): R    = pf(x)
}
