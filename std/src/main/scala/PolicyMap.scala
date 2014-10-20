package psp
package std

import api._, StdShow._

trait VarargsSeq[+A] { def seq: scSeq[A] }

final case class MapLookup[K, V](pf: K ?=> V, defaultValue: Option[V]) {
  def orElse[V1 >: V](that: MapLookup[K, V1]): MapLookup[K, V1] = MapLookup(pf orElse that.pf, defaultValue orElse that.defaultValue)
  def map[V1](f: V => V1): MapLookup[K, V1]                     = MapLookup(pf andThen f, defaultValue map f)
  def apply(key: K): V                                          = if (pf isDefinedAt key) pf(key) else abort(s"$key does not exist")
  def get(key: K): Option[V]                                    = pf lift key orElse defaultValue
  def getOr[V1 >: V](key: K, alt: => V1): V1                    = if (pf isDefinedAt key) pf(key) else alt
}

/** An immutable Map with a fixed iteration order.
 *  It's not a "sorted" map since it has no ordering.
 *  It's true one could say its ordering is Ordering[Int] on indexOf.
 *  Maybe that will seem like a good idea at some point.
 */
final class PolicyMap[K, V](val keySet: exSet[K], private val lookup: MapLookup[K, V]) extends HasSizeInfo with Intensional[K, V] with Extensional[(K, V)] with VarargsSeq[(K, V)] {
  type Entry = (K, V)
  type MapTo[V1] = pMap[K, V1]

  def ++[V1 >: V](map: pMap[K, V1]): MapTo[V1]    = new PolicyMap(keySet union map.keySet, lookup orElse map.lookup)
  def apply(key: K): V                            = lookup(key)
  def contained: pVector[Entry]                   = keyVector map (k => k -> lookup(k))
  def contains(key: K): Boolean                   = keySet(key)
  def filterKeys(p: Predicate[K]): pMap[K, V]     = new PolicyMap(keySet filter p, lookup)
  def filterValues(p: Predicate[V]): pMap[K, V]   = filterKeys(k => p(apply(k)))
  def foreachKey(f: K => Unit): Unit              = keyVector foreach f
  def foreachEntry(f: (K, V) => Unit): Unit       = foreachKey(k => f(k, apply(k)))
  def get(key: K): Option[V]                      = lookup get key
  def getOr[V1 >: V](key: K, alt: => V1): V1      = lookup.getOr(key, alt)
  def isEmpty                                     = contained.isEmpty
  def iterator: BiIterator[Entry]                 = keysIterator map (k => (k, lookup(k)))
  def keyVector: pVector[K]                       = keySet.contained.pvec
  def keys: pVector[K]                            = keyVector
  def keysIterator: BiIterator[K]                 = keyVector.biIterator
  def map[V1](f: V => V1): MapTo[V1]              = new PolicyMap(keySet, lookup map f)
  def reverseKeys                                 = new PolicyMap(keySet mapContained (_.pvec.reverse), lookup)
  def seq: scSeq[Entry]                           = contained.seq
  def sizeInfo: PreciseSize                       = keyVector.sizeInfo
  def values: pVector[V]                          = keyVector map (x => lookup(x))
  def valuesIterator: BiIterator[V]               = keysIterator map (x => lookup(x))
  def withDefaultValue[V1 >: V](v: V1): MapTo[V1] = new PolicyMap(keySet, lookup.copy(defaultValue = Some(v)))
  def toPartial: K ?=> V                          = newPartial(contains, apply)
}

object PolicyMap {
  type BuildsMap[K, V] = Builds[(K, V), pMap[K, V]]

  def builder[K : HashEq, V] : BuildsMap[K, V]             = Direct.builder[(K, V)] map (kvs => new PolicyMap(kvs.m.lefts.pset, MapLookup(kvs.toScalaMap[K, V], None)))
  def apply[K, V](keys: exSet[K], pf: K ?=> V): pMap[K, V] = new PolicyMap(keys, MapLookup(pf, None))
  def unapplySeq[K, V](map: pMap[K, V]): Some[sciSeq[K]]   = Some(map.keyVector.seq)

  /** An immutable scala Map with keys and values in parallel vectors.
   *  It is a "sorted" map in the sense that whatever order the keys are in, that's the sort.
   */
  final class ToScala[K, +V](override val keys: sciVector[K], override val values: sciVector[V])
        extends scala.collection.immutable.SortedMap[K, V]
           with scala.collection.SortedMapLike[K, V, ToScala[K,V]]
           with scala.collection.MapLike[K, V, ToScala[K,V]]
           with RearSliceable[ToScala[K, V]] {
    self =>

    private[this] type Pair = ((K, V))
    private[this] type Us = ToScala[K, V]

    implicit def ordering: Ordering[K]                          = Ordering[Int] on keyIndex
    override def empty: Us                                      = ToScala.empty[K, V]
    override protected[this] def newBuilder : Builder[Pair, Us] = ToScala.newBuilder[K, V]

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

    def drop(n: PreciseSize): Us       = new Us(keys drop n.intSize, values drop n.intSize)
    def take(n: PreciseSize): Us       = new Us(keys take n.intSize, values take n.intSize)
    def slice(range: IndexRange): Us   = new Us(keys.slice(range.startInt, range.endInt), values.slice(range.startInt, range.endInt))
    def dropRight(n: PreciseSize): Us  = new Us(keys dropRight n.intSize, values dropRight n.intSize)
    def takeRight(n: PreciseSize): Us  = new Us(keys takeRight n.intSize, values takeRight n.intSize)

    override def drop(n: Int): Us      = drop(newSize(n))
    override def take(n: Int): Us      = take(newSize(n))
    override def dropRight(n: Int): Us = dropRight(newSize(n))
    override def takeRight(n: Int): Us = takeRight(newSize(n))

    override def takeWhile(p: Predicate[Pair]): Us = (pairs indexWhere !p requiring (_ >= 0)).fold(this)(take)
    override def dropWhile(p: Predicate[Pair]): Us = (pairs indexWhere !p requiring (_ >= 0)).fold(empty)(drop)

    def reverseKeys: Us = new ToScala(keys.reverse, values.reverse)
  }

  object ToScala {
    def newBuilder[K, V] : Builder[(K, V), ToScala[K, V]]    = vectorBuilder[(K, V)]() mapResult apply
    def apply[K, V](pairs: sciVector[(K, V)]): ToScala[K, V] = new ToScala[K, V](pairs map (_._1), pairs map (_._2))
    def empty[K, V] : ToScala[K, V]                          = apply(Vector())

    implicit def showToScala[K: Show, V: Show] = Show[ToScala[K, V]] { map =>
      def len(k: K) = k.to_s.stripAnsi.length
      val width = map.keys map len max
      def fmt(pad: String, k: K, v: V): String = show"$pad$k: $v"

      map.keys map (k => fmt(" " * (width - len(k)), k, map(k))) mkString EOL
    }
  }
}

/** TODO - possible map related methods.

def ascendingFrequency: pMap[A, Int]                     = unsortedFrequencyMap |> (_.orderByValue)
def descendingFrequency: pMap[A, Int]                    = ascendingFrequency.reverse
def unsortedFrequencyMap: Map[A, Int]                    = sciMap(toScalaVector groupBy identity mapValues (_.size) toSeq: _*)
def mapFrom[B](f: A => B): pMap[B, A]                    = newMap(toScalaVector map (x => f(x) -> x): _*)
def mapToAndOnto[B, C](k: A => B, v: A => C): pMap[B, C] = toScalaVector |> (xs => newMap(xs map (x => k(x) -> v(x)): _*))
def mapToMapPairs[B, C](f: A => (B, C)): pMap[B, C]      = toScalaVector |> (xs => newMap(xs map f: _*))
def groupBy[B, C](f: A => B)(g: pSeq[A] => C): pMap[B, C] = {
  val buf = scmMap[B, pList[A]]() withDefaultValue newList[A]()
  pseq foreach (x => buf(f(x)) ::= x)
  newMap((buf.toMap mapValues g).toSeq: _*)
}

final class Map[K, V](val map: scMap[K, V]) extends AnyVal {
  def sortedKeys(implicit ord: Order[K])               = map.keys.sorted
  // def orderByKey(implicit ord: Order[K]): pMap[K, V]   = newMap(sortedKeys, map.toMap)
  // def orderByValue(implicit ord: Order[V]): pMap[K, V] = newMap(sortedKeys(ord on map), map.toMap)
}
final class SortedMap[K, V](val map: scSortedMap[K, V]) extends AnyVal {
  private def ord: Order[K]     = Order create map.ordering
  // def reverse: PolicyMap[K, V] = map orderByKey ord.reverse
}

**/
