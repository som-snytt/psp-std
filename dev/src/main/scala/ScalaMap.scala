package psp
package std
package compat

import api._, StdShow._

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
  def slice(range: IndexRange): Us                = this drop range.toDrop take range.toTake
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

  override def drop(n: Int): Us                  = this drop n.size
  override def take(n: Int): Us                  = this take n.size
  override def dropRight(n: Int): Us             = this dropRight n.size
  override def takeRight(n: Int): Us             = this takeRight n.size
  override def takeWhile(p: Predicate[Pair]): Us = (pairs indexWhere !p requiring (_ >= 0)).fold(this)(take)
  override def dropWhile(p: Predicate[Pair]): Us = (pairs indexWhere !p requiring (_ >= 0)).fold(empty)(drop)
}

object ToScala {
  def newBuilder[K, V] : scmBuilder[(K, V), ToScala[K, V]] = sciVector.newBuilder[(K, V)] mapResult apply
  def apply[K, V](pairs: sciVector[(K, V)]): ToScala[K, V] = new ToScala[K, V](pairs map fst, pairs map snd)
  def empty[K, V] : ToScala[K, V]                          = apply(sciVector())

  implicit def showToScala[K: Show, V: Show] = Show[ToScala[K, V]] { map =>
    def len(k: K) = k.to_s.stripAnsi.length
    def fmt(pad: String, k: K, v: V): String = show"$pad$k: $v"

    val width = (map.keys map len).max
    map.keys map (k => fmt(" " * (width - len(k)), k, map(k))) mkString EOL
  }
}
