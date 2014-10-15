package psp
package std

/** Wrapping java classes.
 */
object NullClassLoader extends jClassLoader
object NullInputStream extends InputStream { def read(): Int = -1 }

object JavaComparator {
  final class Impl[A](f: (A, A) => Int) extends Comparator[A] {
    def compare(x: A, y: A): Int = f(x, y)
  }
  def apply[A](f: (A, A) => Int): Comparator[A] = new Impl[A](f)
}

class ConcurrentMapWrapper[K, V](jmap: ConcurrentHashMap[K, V], default: Option[V]) extends scala.collection.concurrent.Map[K, V] {
  def remove(k: K, v: V): Boolean                      = jmap.remove(k, v)
  def replace(k: K, oldvalue: V, newvalue: V): Boolean = jmap.replace(k, oldvalue, newvalue)
  def replace(k: K, v: V): Option[V]                   = Option(jmap.replace(k, v))
  def putIfAbsent(k: K, v: V): Option[V]               = Option(jmap.putIfAbsent(k, v))

  def -=(key: K): this.type        = try this finally jmap remove key
  def +=(kv: (K, V)): this.type    = try this finally jmap.put(kv._1, kv._2)
  def get(key: K): Option[V]       = Option(jmap get key)
  def iterator: BiIterator[(K, V)] = BiIterable(jmap.keySet).iterator map (k => (k, apply(k)))

  override def contains(key: K): Boolean = jmap containsKey key
  override def apply(key: K): V          = get(key) ||? default | noSuchElementException(s"$key")
}
