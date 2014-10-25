package psp
package std

import api._, StdShow._
import Lookup._

trait VarargsSeq[+A] { def seq: scSeq[A] }

final class IntensionalMap[K, V](val keySet: inSet[K], private val lookup: Lookup[K, V]) extends PolicyMap[K, V](keySet, lookup) with InMap[K, V] {
  type This = inMap[K, V]
  def filterKeys(p: Predicate[K]): This = new IntensionalMap(keySet filter p, lookup)
}
final class ExtensionalMap[K, V](val keySet: exSet[K], private val lookup: Lookup[K, V]) extends PolicyMap[K, V](keySet, lookup) with ExMap[K, V] with VarargsSeq[(K, V)] {
  type Entry = (K, V)
  type This  = exMap[K, V]

  private[this] def newMap[V1](keySet: exSet[K], lookup: Lookup[K, V1]): exMap[K, V1] = new ExtensionalMap(keySet, lookup)
  private[this] def newKeys(keySet: exSet[K]): This                                   = newMap(keySet, lookup)
  private[this] def newLookup[V1](lookup: Lookup[K, V1]): exMap[K, V1]                = newMap(keySet, lookup)

  def +(key: K, value: V): This             = newLookup(lookup.put(key, value)(keySet.hashEq))
  def ++(map: This): This                   = newMap(keySet union map.keySet, map.lookup orElse lookup)
  def contained: pVector[Entry]             = keyVector map (k => k -> lookup(k))
  def filterKeys(p: Predicate[K]): This     = newKeys(keySet filter p)
  def foreachEntry(f: (K, V) => Unit): Unit = foreachKey(k => f(k, apply(k)))
  def foreachKey(f: K => Unit): Unit        = keyVector foreach f
  def isEmpty: Boolean                      = contained.isEmpty
  def iterator: BiIterator[Entry]           = keysIterator map (k => (k, lookup(k)))
  def keyVector: pVector[K]                 = keySet.contained.pvec
  def keys: pVector[K]                      = keyVector
  def keysIterator: BiIterator[K]           = keyVector.biIterator
  def map[V1](f: V => V1): exMap[K, V1]     = newLookup(lookup map f)
  def reverseKeys                           = newKeys(keySet mapContained (_.pvec.reverse))
  def seq: scSeq[Entry]                     = contained.seq
  def size: Precise                         = keyVector.size
  def values: pVector[V]                    = keyVector map (x => lookup(x))
  def valuesIterator: BiIterator[V]         = keysIterator map (x => lookup(x))
  def withDefaultValue(v: V): This          = newLookup(lookup withDefault ConstantDefault(v))

  def merge(that: This)(implicit z: Sums[V]): This =
    that.keySet.contained.foldl(this)((res, key) =>
      if (res contains key)
        res + (key, z.sum(res(key), that(key)))
      else
        res + (key, that(key))
    )
}

/** An immutable Map with a fixed iteration order.
 *  It's not a "sorted" map since it has no ordering.
 *  It's true one could say its ordering is Ordering[Int] on indexOf.
 *  Maybe that will seem like a good idea at some point.
 */
sealed abstract class PolicyMap[K, V](keySet: PolicySet[K], lookup: Lookup[K, V]) extends Intensional[K, V] {
  type This <: PolicyMap[K, V]

  def filterKeys(p: Predicate[K]): This
  def filterValues(p: Predicate[V]): This = filterKeys(k => p(apply(k)))
  def apply(key: K): V                    = lookup(key)
  def contains(key: K): Boolean           = keySet(key)
  def get(key: K): Option[V]              = lookup get key
  def getOr(key: K, alt: => V): V         = lookup.getOr(key, alt)
  def partial: K ?=> V                    = newPartial(contains, apply)
}

object PolicyMap {
  type BuildsMap[K, V] = Builds[(K, V), exMap[K, V]]

  def builder[K : HashEq, V] : BuildsMap[K, V]                  = Direct.builder[(K, V)] map (kvs => new ExtensionalMap(kvs.m.lefts.pset, Lookup(kvs.toPartial)))
  def apply[K, V](keys: exSet[K], pf: K ?=> V): exMap[K, V]     = new ExtensionalMap(keys, Lookup(pf))
  def unapplySeq[K, V](map: exMap[K, V]): scala.Some[sciSeq[K]] = Some(map.keyVector.seq)

  /** An immutable scala Map with keys and values in parallel vectors.
   *  It is a "sorted" map in the sense that whatever order the keys are in, that's the sort.
   */
  final class ToScala[K, +V](override val keys: sciVector[K], override val values: sciVector[V])
        extends scala.collection.immutable.SortedMap[K, V]
           with scala.collection.SortedMapLike[K, V, ToScala[K,V]]
           with scala.collection.MapLike[K, V, ToScala[K,V]] {
    self =>

    private[this] type Pair = ((K, V))
    private[this] type Us = ToScala[K, V]

    implicit def ordering: Ordering[K]                          = Ordering[Int] on keyIndex
    override def empty: Us                                      = ToScala.empty[K, V]
    override protected[this] def newBuilder : scmBuilder[Pair, Us] = ToScala.newBuilder[K, V]

    private def keyIndex(key: K)          = keys indexOf key
    private def pairs                     = keys zip values
    private def optIndex(p: Predicate[K]) = keys indexWhere p requiring (_ >= 0)
    private def indicesFrom(start: K): scIterator[Int] = keyIndex(start) match {
      case n if n < 0 => scIterator.empty
      case n          => scIterator.range(n, keys.length)
    }

    def -(key: K): ToScala[K,V]                     = optIndex(_ == key).fold(this)(n => new Us((keys take n) ++ (keys drop n + 1), (values take n) ++ (values drop n + 1)))
    def get(key: K): Option[V]                      = optIndex(_ == key) map values
    def iterator: scIterator[Pair]                  = keys.iterator zip values.iterator
    def keysIteratorFrom(start: K): scIterator[K]   = indicesFrom(start) map keys
    def valuesIteratorFrom(start: K): scIterator[V] = indicesFrom(start) map values
    def iteratorFrom(start: K): scIterator[Pair]    = indicesFrom(start) map (i => keys(i) -> values(i))

    def rangeImpl(from: Option[K], until: Option[K]): Us =
      (from map keyIndex, until map keyIndex) match {
        case (Some(n), _) if n < 0 => abort(s"Error: rangeImpl($from, $until)")
        case (_, Some(n)) if n < 0 => abort(s"Error: rangeImpl($from, $until)")
        case (Some(s), Some(e))    => slice(s, e)
        case (Some(s), None)       => drop(s)
        case (None, Some(e))       => take(e)
        case _                     => this
      }

    def drop(n: Precise): Us         = new Us(keys drop n.intSize, values drop n.intSize)
    def take(n: Precise): Us         = new Us(keys take n.intSize, values take n.intSize)
    def slice(range: IndexRange): Us = new Us(keys.slice(range.startInt, range.endInt), values.slice(range.startInt, range.endInt))
    def dropRight(n: Precise): Us    = new Us(keys dropRight n.intSize, values dropRight n.intSize)
    def takeRight(n: Precise): Us    = new Us(keys takeRight n.intSize, values takeRight n.intSize)

    override def drop(n: Int): Us      = drop(newSize(n))
    override def take(n: Int): Us      = take(newSize(n))
    override def dropRight(n: Int): Us = dropRight(newSize(n))
    override def takeRight(n: Int): Us = takeRight(newSize(n))

    override def takeWhile(p: Predicate[Pair]): Us = (pairs indexWhere !p requiring (_ >= 0)).fold(this)(take)
    override def dropWhile(p: Predicate[Pair]): Us = (pairs indexWhere !p requiring (_ >= 0)).fold(empty)(drop)

    def reverseKeys: Us = new ToScala(keys.reverse, values.reverse)
  }

  object ToScala {
    def newBuilder[K, V] : scmBuilder[(K, V), ToScala[K, V]]    = vectorBuilder[(K, V)]() mapResult apply
    def apply[K, V](pairs: sciVector[(K, V)]): ToScala[K, V] = new ToScala[K, V](pairs map (_._1), pairs map (_._2))
    def empty[K, V] : ToScala[K, V]                          = apply(sciVector())

    implicit def showToScala[K: Show, V: Show] = Show[ToScala[K, V]] { map =>
      def len(k: K) = k.to_s.stripAnsi.length
      val width = map.keys map len max
      def fmt(pad: String, k: K, v: V): String = show"$pad$k: $v"

      map.keys map (k => fmt(" " * (width - len(k)), k, map(k))) mkString EOL
    }
  }
}

object PolicyMutableMap {
  def apply[K, V: Zero](jmap: jConcurrentMap[K, V]): PolicyMutableMap[K, V] =
    new PolicyMutableMap[K, V](jmap, ConstantDefault(zero[V]))
}

/** Ugh.
 */
class PolicyMutableMap[K, V](jmap: jConcurrentMap[K, V], default: Default[K, V]) extends Intensional[K, V] with AndThis {
  def this(jmap: jConcurrentMap[K, V]) = this(jmap, NoDefault)

  def update(key: K, value: V)         = andThis(jmap.put(key, value))
  def clear()                          = andThis(jmap.clear())
  def containsValue(value: V): Boolean = jmap containsValue value
  def keySet: exSet[K]                 = jmap.keySet.m.naturalSet

  def remove(k: K, v: V): Boolean                      = jmap.remove(k, v)
  def replace(k: K, oldvalue: V, newvalue: V): Boolean = jmap.replace(k, oldvalue, newvalue)
  def replace(k: K, v: V): Option[V]                   = Option(jmap.replace(k, v))
  def putIfAbsent(k: K, v: V): Option[V]               = Option(jmap.putIfAbsent(k, v))

  def -=(key: K): this.type        = try this finally jmap remove key
  def +=(kv: (K, V)): this.type    = try this finally jmap.put(kv._1, kv._2)
  def get(key: K): Option[V]       = Option(jmap get key)
  def iterator: BiIterator[(K, V)] = BiIterable(jmap.keySet).iterator map (k => (k, apply(k)))
  def contains(key: K): Boolean    = jmap containsKey key
  def apply(key: K): V             = get(key) | default(key)
  def withDefaultValue(value: V)   = new PolicyMutableMap(jmap, ConstantDefault(value))
}

/** TODO - possible map related methods.

def ascendingFrequency: exMap[A, Int]                     = unsortedFrequencyMap |> (_.orderByValue)
def descendingFrequency: exMap[A, Int]                    = ascendingFrequency.reverse
def unsortedFrequencyMap: Map[A, Int]                     = sciMap(toScalaVector groupBy identity mapValues (_.size) toSeq: _*)
def mapFrom[B](f: A => B): exMap[B, A]                    = newMap(toScalaVector map (x => f(x) -> x): _*)
def mapToAndOnto[B, C](k: A => B, v: A => C): exMap[B, C] = toScalaVector |> (xs => newMap(xs map (x => k(x) -> v(x)): _*))
def mapToMapPairs[B, C](f: A => (B, C)): exMap[B, C]      = toScalaVector |> (xs => newMap(xs map f: _*))
def groupBy[B, C](f: A => B)(g: pSeq[A] => C): exMap[B, C] = {
  val buf = scmMap[B, pList[A]]() withDefaultValue newList[A]()
  pseq foreach (x => buf(f(x)) ::= x)
  newMap((buf.toMap mapValues g).toSeq: _*)
}

final class Map[K, V](val map: scMap[K, V]) extends AnyVal {
  def sortedKeys(implicit ord: Order[K])               = map.keys.sorted
  // def orderByKey(implicit ord: Order[K]): exMap[K, V]   = newMap(sortedKeys, map.toMap)
  // def orderByValue(implicit ord: Order[V]): exMap[K, V] = newMap(sortedKeys(ord on map), map.toMap)
}
final class SortedMap[K, V](val map: scSortedMap[K, V]) extends AnyVal {
  private def ord: Order[K]     = Order create map.ordering
  // def reverse: PolicyMap[K, V] = map orderByKey ord.reverse
}

**/
