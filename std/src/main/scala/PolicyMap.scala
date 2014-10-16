package psp
package std

import api._

final case class MapLookup[K, V](pf: K ?=> V, defaultValue: Option[V]) extends (K => V) {
  def mapValues[V1](f: V => V1): MapLookup[K, V1] = MapLookup(pf andThen f, defaultValue map f)
  def apply(key: K): V                            = (pf lift key) | abort(s"$key does not exist")
  def get(key: K): Option[V]                      = pf lift key orElse defaultValue
}

/** An immutable Map with a fixed iteration order.
 *  It's not a "sorted" map since it has no ordering.
 *  It's true one could say its ordering is Ordering[Int] on indexOf.
 *  Maybe that will seem like a good idea at some point.
 */
final class PolicyMap[K, +V](val contained: pVector[K], lookup: MapLookup[K, V]) extends HasPreciseSize { //extends Foreach[K] with HasPreciseSize with Intensional[K, V] with Extensional[K] {
  def keys: pVector[K]                              = contained
  def values: pVector[V]                            = keys map lookup
  def contains(key: K)                              = keys containsByEquals key
  def apply(key: K): V                              = lookup(key)
  def withDefaultValue[V1 >: V](v: V1): pMap[K, V1] = new PolicyMap(keys, lookup.copy(defaultValue = Some(v)))
  def foreach(f: K => Unit): Unit                   = keys foreach f
  def size                                          = keys.size
  def reverse                                       = new PolicyMap(keys.reverse, lookup)
  def get(key: K): Option[V]                        = lookup get key

  def keysIterator: BiIterator[K]                   = keys.biIterator
  def valuesIterator: BiIterator[V]                 = keysIterator map lookup
  def iterator: BiIterator[(K, V)]                  = keysIterator map (k => (k, lookup(k)))

  def filterKeys(p: K => Boolean): pMap[K, V]   = new PolicyMap(keys filter p, lookup)
  def filterValues(p: V => Boolean): pMap[K, V] = filterKeys(k => p(apply(k)))

  def mapValues[V1](f: V => V1) = new PolicyMap(keys, lookup mapValues f)
  def pairSeq: pVector[(K, V)]  = keys map (k => k -> lookup(k))
}

object PolicyMap {
  def builder[K, V] : Builds[(K, V), PolicyMap[K, V]] =
    Direct.builder[(K, V)] map (kvs => new PolicyMap[K, V](kvs map (_._1), MapLookup(kvs.toScalaMap[K, V], None)))

  def apply[K, V](keys: pVector[K], pf: K ?=> V): PolicyMap[K, V] = new PolicyMap(keys, MapLookup(pf, None))
}
