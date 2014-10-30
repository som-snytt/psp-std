package psp
package std

import api._, StdShow._
import Lookup._

final class IntensionalMap[K, V](val domain: InSet[K], val lookup: Lookup[K, V]) extends PolicyMap[InSet, K, V](domain, lookup) with InMap[K, V] {
  type NewMap[K1, V1] = InMap[K1, V1]

  protected[this] def newDomain(p: Predicate[K]) = domain filter p
  protected[this] def newMap[K1, V1](domain: InSet[K1], lookup: Lookup[K1, V1]): IntensionalMap[K1, V1] = new IntensionalMap(domain, lookup)
}
final class ExtensionalMap[K, V](val domain: ExSet[K], val lookup: Lookup[K, V]) extends PolicyMap[ExSet, K, V](domain, lookup) with ExMap[K, V] {
  type Entry          = (K, V)
  type NewMap[K1, V1] = ExMap[K1, V1]

  protected[this] def newDomain(p: Predicate[K]) = domain filter p
  protected[this] def newMap[K1, V1](domain: ExSet[K1], lookup: Lookup[K1, V1]): ExtensionalMap[K1, V1] = new ExtensionalMap(domain, lookup)

  def +(key: K, value: V): This             = newMap(domain, lookup.put(key, value)(domain.hashEq))
  def ++(map: This): This                   = newMap(domain union map.domain, map.lookup orElse lookup)
  def entries: View[Entry]                  = keys mapZip lookup
  def foreach(f: ((K, V)) => Unit): Unit    = foreachKey(k => f(k -> apply(k)))
  def foreachEntry(f: (K, V) => Unit): Unit = foreachKey(k => f(k, apply(k)))
  def foreachKey(f: K => Unit): Unit        = keys foreach f
  def isEmpty: Boolean                      = domain.isEmpty
  def iterator: scIterator[Entry]           = keysIterator map (k => (k, lookup(k)))
  def keyVector: Direct[K]                  = keys.pvec
  def keys: View[K]                         = domain
  def keysIterator: scIterator[K]           = keys.iterator
  def reverseKeys                           = newMap(domain.reverse, lookup)
  def seq: scSeq[Entry]                     = entries.seq
  def size: Precise                         = keyVector.size
  def values: View[V]                       = keys map (x => lookup(x))
  def valuesIterator: scIterator[V]         = keysIterator map (x => lookup(x))

  def merge(that: This)(implicit z: Sums[V]): This =
    that.domain.foldl(this)((res, key) =>
      if (res contains key)
        res + (key, z.sum(res(key), that(key)))
      else
        res + (key, that(key))
    )
}

sealed abstract class PolicyMap[Domain[X] <: InSet[X], K, V](domain: Domain[K], lookup: Lookup[K, V]) extends InMap[K, V] {
  type NewMap[K1, V1] <: InMap[K1, V1]
  type This = NewMap[K, V]

  protected[this] def newDomain(p: Predicate[K]): Domain[K]
  protected[this] def newMap[K1, V1](domain: Domain[K1], lookup: Lookup[K1, V1]): NewMap[K1, V1]

  def apply(key: K): V                     = lookup(key)
  def contains(key: K): Boolean            = domain(key)
  def filterKeys(p: Predicate[K]): This    = newMap(newDomain(p), lookup)
  def filterValues(p: Predicate[V]): This  = filterKeys(k => p(apply(k)))
  def get(key: K): Option[V]               = lookup get key
  def getOr(key: K, alt: => V): V          = lookup.getOr(key, alt)
  def map[V1](f: V => V1): NewMap[K, V1]   = newMap(domain, lookup map f)
  def partial: K ?=> V                     = newPartial(contains, apply)
  def withDefaultFunction(f: K => V): This = newMap(domain, lookup withDefault FunctionDefault(f))
  def withDefaultValue(v: V): This         = newMap(domain, lookup withDefault ConstantDefault(v))
  def withNoDefault: This                  = newMap(domain, lookup withDefault NoDefault)
}

/** Ugh.
 */
class PolicyMutableMap[K, V](jmap: jConcurrentMap[K, V], default: Default[K, V]) extends Intensional[K, V] with AndThis {
  def this(jmap: jConcurrentMap[K, V]) = this(jmap, NoDefault)

  def +=(kv: (K, V)): this.type                        = andThis(jmap.put(kv._1, kv._2))
  def -=(key: K): this.type                            = andThis(jmap remove key)
  def apply(key: K): V                                 = get(key) | default(key)
  def clear()                                          = andThis(jmap.clear())
  def contains(key: K): Boolean                        = jmap containsKey key
  def containsValue(value: V): Boolean                 = jmap containsValue value
  def domain: ExSet[K]                                 = jmap.keySet.m.naturalSet
  def get(key: K): Option[V]                           = Option(jmap get key)
  def iterator: scIterator[(K, V)]                     = (domain mapZip apply).iterator
  def putIfAbsent(k: K, v: V): Option[V]               = Option(jmap.putIfAbsent(k, v))
  def remove(k: K, v: V): Boolean                      = jmap.remove(k, v)
  def replace(k: K, oldvalue: V, newvalue: V): Boolean = jmap.replace(k, oldvalue, newvalue)
  def replace(k: K, v: V): Option[V]                   = Option(jmap.replace(k, v))
  def update(key: K, value: V)                         = andThis(jmap.put(key, value))
  def withDefaultFunction(f: K => V)                   = new PolicyMutableMap(jmap, FunctionDefault(f))
  def withDefaultValue(value: V)                       = new PolicyMutableMap(jmap, ConstantDefault(value))
}

object PolicyMap {
  type BuildsMap[K, V] = Builds[(K, V), ExMap[K, V]]

  def builder[K : HashEq, V] : BuildsMap[K, V]                  = Direct.builder[(K, V)] map (kvs => new ExtensionalMap(kvs.m.lefts.pset, Lookup(kvs.toPartial)))
  def apply[K, V](keys: ExSet[K], pf: K ?=> V): ExMap[K, V]     = new ExtensionalMap(keys, Lookup(pf))
  def unapplySeq[K, V](map: ExMap[K, V]): scala.Some[sciSeq[K]] = Some(map.keyVector.seq)

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

    implicit def ordering: Ordering[K] = orderBy[K](keyIndex).toScalaOrdering
    override def empty: Us             = ToScala.empty[K, V]

    override protected[this] def newBuilder : scmBuilder[Pair, Us] = ToScala.newBuilder[K, V]

    private def keyIndex(key: K)          = keys indexOf key
    private def pairs                     = keys zip values
    private def optIndex(p: Predicate[K]) = keys indexWhere p requiring (_ >= 0)
    private def indicesFrom(start: K): scIterator[Int] = keyIndex(start) match {
      case n if n < 0 => scIterator.empty
      case n          => scIterator.range(n, keys.length)
    }

    def -(key: K): ToScala[K,V]                     = optIndex(_ == key).fold(this)(n => new Us((keys take n) ++ (keys drop n + 1), (values take n) ++ (values drop n + 1)))
    def drop(n: Precise): Us                        = new Us(keys drop n.intSize, values drop n.intSize)
    def dropRight(n: Precise): Us                   = new Us(keys dropRight n.intSize, values dropRight n.intSize)
    def get(key: K): Option[V]                      = optIndex(_ == key) map values
    def iterator: scIterator[Pair]                  = keys.iterator zip values.iterator
    def iteratorFrom(start: K): scIterator[Pair]    = indicesFrom(start) map (i => keys(i) -> values(i))
    def keysIteratorFrom(start: K): scIterator[K]   = indicesFrom(start) map keys
    def reverseKeys: Us                             = new ToScala(keys.reverse, values.reverse)
    def slice(range: IndexRange): Us                = this drop range.precedingSize take range.size
    def take(n: Precise): Us                        = new Us(keys take n.intSize, values take n.intSize)
    def takeRight(n: Precise): Us                   = new Us(keys takeRight n.intSize, values takeRight n.intSize)
    def valuesIteratorFrom(start: K): scIterator[V] = indicesFrom(start) map values

    def rangeImpl(from: Option[K], until: Option[K]): Us =
      (from map keyIndex, until map keyIndex) match {
        case (Some(n), _) if n < 0 => abort(s"Error: rangeImpl($from, $until)")
        case (_, Some(n)) if n < 0 => abort(s"Error: rangeImpl($from, $until)")
        case (Some(s), Some(e))    => slice(s, e)
        case (Some(s), None)       => drop(s)
        case (None, Some(e))       => take(e)
        case _                     => this
      }


    override def drop(n: Int): Us                  = drop(newSize(n))
    override def take(n: Int): Us                  = take(newSize(n))
    override def dropRight(n: Int): Us             = dropRight(newSize(n))
    override def takeRight(n: Int): Us             = takeRight(newSize(n))
    override def takeWhile(p: Predicate[Pair]): Us = (pairs indexWhere !p requiring (_ >= 0)).fold(this)(take)
    override def dropWhile(p: Predicate[Pair]): Us = (pairs indexWhere !p requiring (_ >= 0)).fold(empty)(drop)
  }

  object ToScala {
    def newBuilder[K, V] : scmBuilder[(K, V), ToScala[K, V]] = sciVector.newBuilder[(K, V)] mapResult apply
    def apply[K, V](pairs: sciVector[(K, V)]): ToScala[K, V] = new ToScala[K, V](pairs map (_._1), pairs map (_._2))
    def empty[K, V] : ToScala[K, V]                          = apply(sciVector())

    implicit def showToScala[K: Show, V: Show] = Show[ToScala[K, V]] { map =>
      def len(k: K) = k.to_s.stripAnsi.length
      def fmt(pad: String, k: K, v: V): String = show"$pad$k: $v"

      val width = (map.keys map len).max
      map.keys map (k => fmt(" " * (width - len(k)), k, map(k))) mkString EOL
    }
  }
}

object PolicyMutableMap {
  def apply[K, V: Zero](jmap: jConcurrentMap[K, V]): PolicyMutableMap[K, V] =
    new PolicyMutableMap[K, V](jmap, ConstantDefault(zero[V]))
}
