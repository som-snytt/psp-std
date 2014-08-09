package psp
package std

import scala.collection.immutable

/** An immutable Map with a fixed iteration order.
 *  It's not a "sorted" map since it has no ordering.
 *  It's true one could say its ordering is Ordering[Int] on indexOf.
 *  Maybe that will seem like a good idea at some point.
 */
final class OrderedMap[K, +V](override val keys: Seq[K], map: immutable.Map[K, V]) extends immutable.Map[K, V] {
  def reverse                                 = new OrderedMap(keys.reverse, map)
  def +[V1 >: V](kv: (K, V1))                 = new OrderedMap(keys :+ kv._1, map + kv)
  def -(key: K)                               = new OrderedMap(keys filterNot (_ == key), map - key)
  def get(key: K): Option[V]                  = map get key
  def iterator: Iterator[(K, V)]              = keys.iterator map (k => (k, map(k)))
  override def keysIterator                   = keys.iterator
  override def filter(p: ((K, V)) => Boolean) = new OrderedMap(keys filter (k => p(k -> map(k))), map)
  override def filterKeys(p: K => Boolean)    = new OrderedMap(keys filter p, map)
}
