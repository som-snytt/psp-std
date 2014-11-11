package psp
package std

import api._, Lookup._

object ExMap {
  type BuildsMap[K, V] = Builds[(K, V), ExMap[K, V]]

  def builder[K : HashEq, V] : BuildsMap[K, V]              = Direct.builder[(K, V)] map (kvs => new Impl(kvs.m.lefts.toExSet, Lookup(kvs.toPartial)))
  def apply[K, V](keys: ExSet[K], pf: K ?=> V): ExMap[K, V] = new Impl(keys, Lookup(pf))
  def unapplySeq[K, V](map: ExMap[K, V]): Some[sciSeq[K]]   = Some(map.keyVector.seq)
  def impl[K, V](xs: ExMap[K, V]): Impl[K, V] = xs match {
    case xs: Impl[K, V] => xs
    case _              => new Impl(xs.domain, Lookup(xs))
  }

  final class Impl[K, V](domain: ExSet[K], lookup: Lookup[K, V]) extends InMap.InMapImpl[ExSet, K, V](domain, lookup) with ExMap[K, V] {
    type Entry          = (K, V)
    type NewMap[K1, V1] = ExMap[K1, V1]

    protected[this] def newDomain(p: Predicate[K])                                              = domain filter p
    protected[this] def newMap[K1, V1](domain: ExSet[K1], lookup: Lookup[K1, V1]): Impl[K1, V1] = new Impl(domain, lookup)

    def +(key: K, value: V): This             = newMap(domain, lookup.put(key, value)(domain.hashEq))
    def entries: View[Entry]                  = keys mapZip lookup
    def foreach(f: ((K, V)) => Unit): Unit    = foreachKey(k => f(k -> apply(k)))
    def foreachEntry(f: (K, V) => Unit): Unit = foreachKey(k => f(k, apply(k)))
    def foreachKey(f: K => Unit): Unit        = keys foreach f
    def isEmpty: Boolean                      = domain.isEmpty
    def iterator: scIterator[Entry]           = keysIterator map (k => (k, lookup(k)))
    def keyVector: Direct[K]                  = keys.toDirect
    def keys: View[K]                         = domain
    def keysIterator: scIterator[K]           = keys.iterator
    def reverseKeys                           = newMap(domain.reverse, lookup)
    def seq: scSeq[Entry]                     = entries.seq
    def size: Precise                         = keyVector.size
    def values: View[V]                       = keys map (x => lookup(x))
    def valuesIterator: scIterator[V]         = keysIterator map (x => lookup(x))

    def merge(that: This)(implicit z: Sums[V]): This = that.domain.foldl(this)((res, key) =>
      if (res contains key)
        res + (key, z.sum(res(key), that(key)))
      else
        res + (key, that(key))
    )
  }
}
object InMap {
  def apply[K, V](keys: InSet[K], f: K => V): InMap[K, V] = new Impl(keys, Lookup total f)
  def impl[K, V](xs: InMap[K, V]): Impl[K, V] = xs match {
    case xs: Impl[K, V] => xs
    case _              => new Impl(xs.domain, Lookup(xs))
  }

  sealed abstract class InMapImpl[Domain[X] <: InSet[X], K, V](val domain: Domain[K], val lookup: Lookup[K, V]) extends InMap[K, V] {
    type NewMap[K1, V1] <: InMap[K1, V1]
    type This = NewMap[K, V]

    protected[this] def newDomain(p: Predicate[K]): Domain[K]
    protected[this] def newMap[K1, V1](domain: Domain[K1], lookup: Lookup[K1, V1]): NewMap[K1, V1]

    def apply(key: K): V                     = lookup(key)
    def filterKeys(p: Predicate[K]): This    = newMap(newDomain(p), lookup)
    def filterValues(p: Predicate[V]): This  = filterKeys(k => p(apply(k)))
    def get(key: K): Option[V]               = lookup get key
    def getOr(key: K, alt: => V): V          = lookup.getOr(key, alt)
    def map[V1](f: V => V1): NewMap[K, V1]   = newMap(domain, lookup map f)
    def withDefaultFunction(f: K => V): This = newMap(domain, lookup withDefault FunctionDefault(f))
    def withDefaultValue(v: V): This         = newMap(domain, lookup withDefault ConstantDefault(v))
    def withNoDefault: This                  = newMap(domain, lookup withDefault NoDefault)
  }

  final class Impl[K, V](domain: InSet[K], lookup: Lookup[K, V]) extends InMapImpl[InSet, K, V](domain, lookup) with InMap[K, V] {
    type NewMap[K1, V1] = InMap[K1, V1]

    protected[this] def newDomain(p: Predicate[K])                                              = domain filter p
    protected[this] def newMap[K1, V1](domain: InSet[K1], lookup: Lookup[K1, V1]): Impl[K1, V1] = new Impl(domain, lookup)
  }
}
