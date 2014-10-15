package psp
package std

import java.{ lang => jl }
import api._

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
    def sortedKeys(implicit ord: Order[K])                     = map.keys.toVector.sorted(ordering[K])
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
    def toForeach: Foreach[A]           = each(toTraversable)
    def toTraversable: scTraversable[A] = toScalaIterator.toTraversable
    def toScalaIterator: scIterator[A]  = new ScalaIterator(xs.iterator)
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

    def foreachCounted(f: (Index, A) => Unit): Unit        = foldl(0.index)((idx, x) => try idx.next finally f(idx, x))
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

    def toIterable: scIterable[A]       = toScala[scIterable]
    def toList: sciList[A]              = toScala[sciList]
    def toScalaSet: sciSet[A]           = toScala[sciSet]
    def toSeq: sciSeq[A]                = toScala[sciSeq]
    def toStream: sciStream[A]          = toScala[sciStream]
    def toTraversable: scTraversable[A] = toScala[scTraversable]
    def toVector: sciVector[A]          = toScala[sciVector]
    def toScalaVector: sciVector[A]     = toVector

    def trav: scTraversable[A]       = toTraversable
    def scalaIterator: scIterator[A] = toIterable.iterator
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
    def isDigit      = jl.Character isDigit ch
    def toUpper      = jl.Character toUpperCase ch
    def toLower      = jl.Character toLowerCase ch
    def isUpper      = jl.Character isUpperCase ch
    def isLower      = jl.Character isLowerCase ch
    def isWhitespace = jl.Character isWhitespace ch
    def isControl    = jl.Character isISOControl ch
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
}

final class LabeledFunction[-T, +R](f: T => R, val to_s: String) extends (T => R) with ShowDirectNow {
  def apply(x: T): R = f(x)
}
final class LabeledPartialFunction[-T, +R](pf: PartialFunction[T, R], val to_s: String) extends PartialFunction[T, R] with ShowDirectNow {
  def isDefinedAt(x: T) = pf isDefinedAt x
  def apply(x: T): R    = pf(x)
}

final class ScalaIterator[A](xs: jIterator[A]) extends scala.Iterator[A] {
  def next    = xs.next
  def hasNext = xs.hasNext
}
