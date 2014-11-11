package psp
package std

import api._, Lookup._

trait MutableMap[K, V] extends Intensional[K, V] with AndThis {
  def +=(kv: (K, V)): this.type
  def -=(key: K): this.type
  def apply(key: K): V
  def clear(): this.type
  def contains(key: K): Boolean
  def containsValue(value: V): Boolean
  def domain: ExSet[K]
  def get(key: K): Option[V]
  def iterator: scIterator[(K, V)]
  def putIfAbsent(k: K, v: V): Option[V]
  def remove(k: K, v: V): Boolean
  def replace(k: K, oldvalue: V, newvalue: V): Boolean
  def replace(k: K, v: V): Option[V]
  def update(key: K, value: V): this.type
  def withDefaultFunction(f: K => V): MutableMap[K, V]
  def withDefaultValue(value: V): MutableMap[K, V]
}

object MutableMap {
  def apply[K, V](jmap: jConcurrentMap[K, V]): WrapConcurrent[K, V]                         = apply[K, V](jmap, NoDefault)
  def apply[K, V](jmap: jConcurrentMap[K, V], default: Default[K, V]): WrapConcurrent[K, V] = new WrapConcurrent(jmap, default)

  final class WrapConcurrent[K, V](jmap: jConcurrentMap[K, V], default: Default[K, V]) extends MutableMap[K, V] {
    def +=(kv: (K, V)): this.type                        = andThis(jmap.put(kv._1, kv._2))
    def -=(key: K): this.type                            = andThis(jmap remove key)
    def apply(key: K): V                                 = get(key) | default(key)
    def clear()                                          = andThis(jmap.clear())
    def contains(key: K): Boolean                        = jmap containsKey key
    def containsValue(value: V): Boolean                 = jmap containsValue value
    def domain: ExSet[K]                                 = jmap.keySet.byEquals.toSet
    def get(key: K): Option[V]                           = Option(jmap get key)
    def iterator: scIterator[(K, V)]                     = (domain mapZip apply).iterator
    def putIfAbsent(k: K, v: V): Option[V]               = Option(jmap.putIfAbsent(k, v))
    def remove(k: K, v: V): Boolean                      = jmap.remove(k, v)
    def replace(k: K, oldvalue: V, newvalue: V): Boolean = jmap.replace(k, oldvalue, newvalue)
    def replace(k: K, v: V): Option[V]                   = Option(jmap.replace(k, v))
    def update(key: K, value: V)                         = andThis(jmap.put(key, value))
    def withDefaultFunction(f: K => V)                   = new WrapConcurrent(jmap, FunctionDefault(f))
    def withDefaultValue(value: V)                       = new WrapConcurrent(jmap, ConstantDefault(value))
  }
}
