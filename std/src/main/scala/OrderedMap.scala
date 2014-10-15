package psp
package std

import api._

/** An immutable Map with a fixed iteration order.
 *  It's not a "sorted" map since it has no ordering.
 *  It's true one could say its ordering is Ordering[Int] on indexOf.
 *  Maybe that will seem like a good idea at some point.
 */
final class OrderedMap[K, +V](override val keys: sciSeq[K], map: sciMap[K, V]) extends sciMap[K, V] {
  def reverse                                 = new OrderedMap(keys.reverse, map)
  def +[V1 >: V](kv: (K, V1))                 = new OrderedMap(keys :+ kv._1, map + kv)
  def -(key: K)                               = new OrderedMap(keys filterNot (_ == key), map - key)
  def get(key: K): Option[V]                  = map get key
  def iterator: scIterator[(K, V)]            = keys.iterator map (k => (k, map(k)))
  override def keysIterator                   = keys.iterator
  override def filter(p: ((K, V)) => Boolean) = new OrderedMap(keys filter (k => p(k -> map(k))), map)
  override def filterKeys(p: K => Boolean)    = new OrderedMap(keys filter p, map)
}

object OrderedMap {
  implicit def ShowOrderedMap[K: Show, V: Show] = Show[OrderedMap[K, V]] { map =>
    def len(k: K) = k.to_s.stripAnsi.length
    val width = map.keys map len max
    def fmt(pad: String, k: K, v: V): String = show"$pad$k: $v"

    map.keys map (k => fmt(" " * (width - len(k)), k, map(k))) mkString EOL
  }
}
