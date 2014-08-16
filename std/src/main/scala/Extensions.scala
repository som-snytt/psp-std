package psp
package std

import scala.collection.{ mutable, immutable, generic }

// Have to each go into their own class because the apply methods have the same erasure.
final class AddNthApplyToSeq[CC[X] <: scala.collection.Seq[X], A](private val xs: CC[A]) extends AnyVal {
  def apply(nth: Nth): A = if (nth.isUndefined) sys.error(s"apply($nth)") else xs(nth.intIndex)
}
final class AddIndexApplyToSeq[CC[X] <: scala.collection.Seq[X], A](private val xs: CC[A]) extends AnyVal {
  def apply(index: Index): A = if (index.isUndefined) sys.error(s"apply($index)") else xs(index.value)
}

final class SortedMapExtensionOps[K, V](private val map: scala.collection.SortedMap[K, V]) extends AnyVal {
  private def ord = map.ordering
  def reverse: OrderedMap[K, V] = map orderByKey ord.reverse
}

final class MapExtensionOps[K, V](private val map: scala.collection.Map[K, V]) extends AnyVal {
  def orderByKey(implicit ord: Ordering[K]): OrderedMap[K, V]   = orderedMap(map.keys.toSeq.sorted, map.toMap)
  def orderByValue(implicit ord: Ordering[V]): OrderedMap[K, V] = orderedMap(map.keys.toSeq sorted (ord on map), map.toMap)
}

final class GenTraversableOnceExtensionOps[CC[X] <: scala.collection.GenTraversableOnce[X], A](private val xs: CC[A]) extends AnyVal {
  def sortDistinct(implicit ord: Ordering[A]): Vector[A]         = distinct.sorted
  def mapFrom[B](f: A => B): OrderedMap[B, A]                    = orderedMap(xs.toVector map (x => (f(x), x)): _*)
  def mapOnto[B](f: A => B): OrderedMap[A, B]                    = orderedMap(xs.toVector map (x => (x, f(x))): _*)
  def mapToAndOnto[B, C](k: A => B, v: A => C): OrderedMap[B, C] = xs.toVector |> (xs => orderedMap(xs map (x => k(x) -> v(x)): _*))
  def mapToMapPairs[B, C](f: A => (B, C)): OrderedMap[B, C]      = xs.toVector |> (xs => orderedMap(xs map f: _*))
  def sorted(implicit ord: Ordering[A]): Vector[A]               = xs.toVector.sorted
  def distinct: Vector[A]                                        = xs.toVector.distinct
  def unsortedFrequencyMap: Map[A, Int]                          = immutableMap(xs.toVector groupBy (x => x) mapValues (_.size) toSeq: _*)
  def ascendingFrequency: OrderedMap[A, Int]                     = unsortedFrequencyMap |> (_.orderByValue)
  def descendingFrequency: OrderedMap[A, Int]                    = ascendingFrequency.reverse
}

/** We could avoid this duplication with some of scala's incredibly high-cost
 *  (in performance and complexity tax) machinery. I'll take the duplication at this point.
 */
trait SeqLikeExtensionOps[A] extends Any {
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

final class SeqExtensionOps[CC[X] <: scala.collection.Seq[X], A](private val xs: CC[A]) extends AnyVal with SeqLikeExtensionOps[A] {
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

final class ArrayExtensionOps[A](private val xs: Array[A]) extends AnyVal with SeqLikeExtensionOps[A] {
  def length                                   = xs.length
  def index(elem: A): Index                    = Index(xs indexOf elem)
  def lastIndex(elem: A): Index                = Index(xs lastIndexOf elem)
  def indexAtWhich(p: A => Boolean): Index     = Index(xs indexWhere p)
  def lastIndexAtWhich(p: A => Boolean): Index = Index(xs lastIndexWhere p)
  def hasElem(elem: A): Boolean                = xs contains elem

  def apply(range: IndexRange)(implicit tag: ClassTag[A]): Array[A] = (indexRange intersect range) |> (r => xs.slice(r.startInt, r.endInt))
}

final class AnyExtensionOps[A](private val x: A) extends AnyVal {
  // The famed forward pipe.
  @inline def |>[B](f: A => B): B = f(x)

  // "Maybe we can enforce good programming practice with annoyingly long method names."
  def castTo[U] : U         = x.asInstanceOf[U]
  def toRef: AnyRef         = castTo[AnyRef]
  def doto(f: A => Unit): A = try x finally f(x)

  def try_s[A1 >: A](implicit shows: Show[A1] = null): String = if (shows == null) any_s else (x: A1).to_s
  def any_s: String = x match {
    case x: ShowDirect => x.to_s
    case _             => "" + x
  }
}

final class TryExtensionOps[A](private val x: Try[A]) extends AnyVal {
  def fold[B](f: A => B, g: Throwable => B): B = x match {
    case scala.util.Success(x) => f(x)
    case scala.util.Failure(t) => g(t)
  }
}
