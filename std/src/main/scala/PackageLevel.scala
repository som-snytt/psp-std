package psp
package std

import scala.collection.{ mutable, immutable, generic }

trait PackageLevel extends Implicits with Creators

trait Creators {
  def index(x: Int): Index   = Index(x)
  def offset(x: Int): Offset = Offset(x)
  def nth(x: Int): Nth       = Nth(x)

  def mutableSeq[A](xs: A*): mutable.Seq[A]                              = mutable.Seq(xs: _*)
  def mutableSet[A](xs: A*): mutable.Set[A]                              = mutable.Set(xs: _*)
  def mutableMap[K, V](kvs: (K, V)*): mutable.Map[K, V]                  = mutable.Map[K, V](kvs: _*)
  def immutableSeq[A](xs: A*): immutable.Seq[A]                          = immutable.Seq(xs: _*)
  def immutableSet[A](xs: A*): immutable.Set[A]                          = immutable.Set(xs: _*)
  def immutableMap[K, V](kvs: (K, V)*): immutable.Map[K, V]              = immutable.Map[K, V](kvs: _*)

  def orderedMap[K, V](kvs: (K, V)*): OrderedMap[K, V]                 = new OrderedMap[K, V](kvs map (_._1), kvs.toMap)
  def orderedMap[K, V](keys: Seq[K], map: Map[K, V]): OrderedMap[K, V] = new OrderedMap[K, V](keys, map)

  def listBuilder[A](xs: A*)            = List.newBuilder[A] ++= xs
  def arrayBuilder[A: ClassTag](xs: A*) = Array.newBuilder[A] ++= xs
  def vectorBuilder[A](xs: A*)          = Vector.newBuilder[A] ++= xs
}

trait Implicits {
  implicit def stringExtensionOps(s: String): StringExtensionOps            = new StringExtensionOps(s)
  implicit def arrayExtensionOps[A](xs: Array[A]): ArrayExtensionOps[A]     = new ArrayExtensionOps[A](xs)
  implicit def anyExtensionOps[A](x: A): AnyExtensionOps[A]                 = new AnyExtensionOps[A](x)
  implicit def tryExtensionOps[A](x: scala.util.Try[A]): TryExtensionOps[A] = new TryExtensionOps[A](x)

  implicit def sortedMapExtensionOps[K, V](xs: scala.collection.SortedMap[K, V]): SortedMapExtensionOps[K, V]                      = new SortedMapExtensionOps[K, V](xs)
  implicit def mapExtensionOps[K, V](xs: scala.collection.Map[K, V]): MapExtensionOps[K, V]                                        = new MapExtensionOps[K, V](xs)
  implicit def seqExtensionOps[CC[X] <: scala.collection.Seq[X], A](xs: CC[A]): SeqExtensionOps[CC, A]                             = new SeqExtensionOps[CC, A](xs)
  implicit def seqNthExtensionOps[CC[X] <: scala.collection.Seq[X], A](xs: CC[A]): AddNthApplyToSeq[CC, A]                         = new AddNthApplyToSeq[CC, A](xs)
  implicit def seqIndexExtensionOps[CC[X] <: scala.collection.Seq[X], A](xs: CC[A]): AddIndexApplyToSeq[CC, A]                     = new AddIndexApplyToSeq[CC, A](xs)
  implicit def genTraversableOnceExtensionOps[CC[X] <: GenTraversableOnce[X], A](xs: CC[A]): GenTraversableOnceExtensionOps[CC, A] = new GenTraversableOnceExtensionOps[CC, A](xs)
}

// An immutable Map with a fixed iteration order.
// It's not a "sorted" map since it has no ordering.
// It's true one could say its ordering is Ordering[Int] on indexOf.
// Maybe that will seem like a good idea at some point.
final class OrderedMap[K, +V](override val keys: Seq[K], map: Map[K, V]) extends immutable.Map[K, V] {
  def reverse                                 = new OrderedMap(keys.reverse, map)
  def +[V1 >: V](kv: (K, V1))                 = new OrderedMap(keys :+ kv._1, map + kv)
  def -(key: K)                               = new OrderedMap(keys filterNot (_ == key), map - key)
  def get(key: K): Option[V]                  = map get key
  def iterator: Iterator[(K, V)]              = keys.iterator map (k => (k, map(k)))
  override def keysIterator                   = keys.iterator
  override def filter(p: ((K, V)) => Boolean) = new OrderedMap(keys filter (k => p(k -> map(k))), map)
  override def filterKeys(p: K => Boolean)    = new OrderedMap(keys filter p, map)
}

// Have to each go into their own class because the apply methods have the same erasure.
final class AddNthApplyToSeq[CC[X] <: scala.collection.Seq[X], A](private val xs: CC[A]) extends AnyVal {
  def apply(nth: Nth): A = if (nth.isDefined) xs(nth.intIndex) else sys.error(s"apply($nth)")
}
final class AddIndexApplyToSeq[CC[X] <: scala.collection.Seq[X], A](private val xs: CC[A]) extends AnyVal {
  def apply(index: Index): A = if (index.isDefined) xs(index.value) else sys.error(s"apply($index)")
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
  def sortDistinct(implicit ord: Ordering[A]): Vector[A] = distinct.sorted
  def mapOnto[B](f: A => B): OrderedMap[A, B]            = orderedMap(xs.toVector map (x => (x, f(x))): _*)
  def sorted(implicit ord: Ordering[A]): Vector[A]       = xs.toVector.sorted
  def distinct: Vector[A]                                = xs.toVector.distinct
  def unsortedFrequencyMap: Map[A, Int]                  = immutableMap(xs.toVector groupBy (x => x) mapValues (_.size) toSeq: _*)
  def ascendingFrequency: OrderedMap[A, Int]             = unsortedFrequencyMap |> (_.orderByValue)
  def descendingFrequency: OrderedMap[A, Int]            = ascendingFrequency.reverse
}

final class SeqExtensionOps[CC[X] <: scala.collection.Seq[X], A](private val xs: CC[A]) extends AnyVal {
  // An added benefit of these methods is a huge increase in type safety
  // because the sequence is treated as invariant due to an idiosyncrasy of
  // scala's type inference.
  def index(elem: A): Index                    = Index(xs indexOf elem)
  def lastIndex(elem: A): Index                = Index(xs lastIndexOf elem)
  def indexAtWhich(p: A => Boolean): Index     = Index(xs indexWhere p)
  def lastIndexAtWhich(p: A => Boolean): Index = Index(xs lastIndexWhere p)
  def indicesOf: IndexRange                    = IndexRange zeroUntil exclusiveEnd
  def hasElem(elem: A): Boolean                = xs contains elem
  def exclusiveEnd: Index                      = Index(xs.length)
  // Produces a vector containing the elements in the given index range.
  // Ignores indices which don't exist in the target sequence.
  def apply(range: IndexRange): Vector[A] = indicesOf intersect range map (i => xs(i.value))
}

// Another demonstration of scala's boilerplate reduction powers.
final class StringExtensionOps(private val xs: String) extends AnyVal {
  def index(elem: Char): Index                    = Index(xs indexOf elem)
  def lastIndex(elem: Char): Index                = Index(xs lastIndexOf elem)
  def indexAtWhich(p: Char => Boolean): Index     = Index(xs indexWhere p)
  def lastIndexAtWhich(p: Char => Boolean): Index = Index(xs lastIndexWhere p)
  def indicesOf: IndexRange                       = IndexRange zeroUntil exclusiveEnd
  def hasElem(elem: Char): Boolean                = (xs indexOf elem) >= 0
  def exclusiveEnd: Index                         = Index(xs.length)

  def apply(range: IndexRange): String = (indicesOf intersect range) |> (r => xs.slice(r.startInt, r.endInt))
  // indicesOf intersect range map (i => xs(i.value))
}

final class ArrayExtensionOps[A](private val xs: Array[A]) extends AnyVal {
  def index(elem: A): Index                    = Index(xs indexOf elem)
  def lastIndex(elem: A): Index                = Index(xs lastIndexOf elem)
  def indexAtWhich(p: A => Boolean): Index     = Index(xs indexWhere p)
  def lastIndexAtWhich(p: A => Boolean): Index = Index(xs lastIndexWhere p)
  def indicesOf: IndexRange                    = IndexRange zeroUntil exclusiveEnd
  def hasElem(elem: A): Boolean                = xs contains elem
  def exclusiveEnd: Index                      = Index(xs.length)

  def apply(range: IndexRange)(implicit tag: reflect.ClassTag[A]): Array[A] = (indicesOf intersect range) |> (r => xs.slice(r.startInt, r.endInt))
  // indicesOf intersect range map (i => xs(i.value))
}

final class AnyExtensionOps[A](val x: A) extends AnyVal {
  // The famed forward pipe.
  @inline def |>[B](f: A => B): B = f(x)
}

final class TryExtensionOps[A](val x: scala.util.Try[A]) extends AnyVal {
  def fold[B](f: A => B, g: Throwable => B): B = x match {
    case scala.util.Success(x) => f(x)
    case scala.util.Failure(t) => g(t)
  }
}
