package psp
package std
package ops

import api._

final class ArraySpecificOps[A](val xs: Array[A]) extends AnyVal with HasPreciseSizeMethods {
  def size: IntSize = Precise(xs.length)
  private def andThis(op: Unit): xs.type = xs

  def apply(idx: Index): A                   = xs(idx.safeToInt)
  def updated(idx: Index, value: A): xs.type = andThis(xs(idx.safeToInt) = value)
  def mapInPlace(f: A => A): xs.type         = andThis(foreachIndex(i => updated(i, f(apply(i)))))
  def sortInPlace: Array[A]                  = Array sortInPlace xs
}

trait ConversionOps[A] extends Any {
  def xs: Each[A]

  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A]        = z build xs
  def toScala[CC[X]](implicit z: CanBuild[A, CC[A]]): CC[A] = to[CC](Builds wrap z)

  // XXX Let's have all these converters return the internal type,
  // not the API type, as a reward for the verbosity.
  def toPolicyList: pList[A]                                                     = PolicyList.builder[A] build xs
  def toPolicySeq: Each[A]                                                       = Each.builder[A] build xs
  def toPolicySet(implicit z: HashEq[A]): ExtensionalSet[A]                      = PolicySet.builder[A] build xs
  def toPolicyVector: Direct[A]                                                  = Direct.builder[A] build xs
  def toPolicyMap[K: HashEq, V](implicit ev: A <:< (K, V)): ExtensionalMap[K, V] = PolicyMap.builder[K, V] build (xs map ev)

  def toScalaIterable: scIterable[A]                            = toScala[scIterable]
  def toScalaList: sciList[A]                                   = toScala[sciList]
  def toScalaSet: sciSet[A]                                     = toScala[sciSet]
  def toScalaVector: sciVector[A]                               = toScala[sciVector]
  def toScalaSeq: sciSeq[A]                                     = toScala[sciSeq]
  def toScalaStream: sciStream[A]                               = toScala[sciStream]
  def toScalaTraversable: scTraversable[A]                      = toScala[scTraversable]
  def toScalaMap[K, V](implicit ev: A <:< (K, V)): sciMap[K, V] = toScalaVector map ev toMap
  def toArray(implicit z: CTag[A]): Array[A]                    = Array.newBuilder[A] ++= toScalaTraversable result

  def toPartial[K, V](implicit ev: A <:< (K, V)): K ?=> V = toScalaMap[K, V]

  def toJava: jList[A]                                       = jList(seq: _*)
  def toJavaSet: jSet[A]                                     = jSet(seq: _*)
  def toJavaMap[K, V](implicit ev: A <:< (K, V)): jMap[K, V] = jMap(seq map ev: _*)

  def iterator: scIterator[A] = xs match {
    case FromScala(xs: scIterable[A]) => xs.iterator
    case xs: Direct[A]                => BiIterator vector xs
    case xs: Linear[A]                => BiIterator linear xs
    case _                            => xs.memo.iterator
  }
  def plist: pList[A]                                                  = toPolicyList
  def pvec: Direct[A]                                                  = toPolicyVector
  def pseq: Each[A]                                                    = toPolicySeq
  def pset(implicit z: HashEq[A]): ExSet[A]                            = toPolicySet
  def pmap[K, V](implicit ev: A <:< (K, V), z: HashEq[K]): ExMap[K, V] = toPolicyMap[K, V]

  def toSetByEquals: ExSet[A]          = pset(HashEq.natural())
  def toSetByRef: ExSet[AnyRef with A] = xs.toRefs.pset(HashEq.reference())

  def naturalMap[K, V](implicit ev: A <:< (K, V)): ExMap[K, V] = toPolicyMap[K, V](HashEq.natural(), ev)
  def naturalSet: ExSet[A]                                     = toPolicySet(HashEq.natural())

  def seq: sciSeq[A] = toScalaSeq // new Each.ToScala(xs) // varargs
}

final class ForeachOps[A](val xs: Each[A]) extends AnyVal with ConversionOps[A] {
  def sized(size: Precise): Each[A] = new Each.Sized(xs, size)
  def memo: Indexed.Memo[A] = xs match {
    case xs: Indexed.Memo[A] => xs
    case _                   => new Indexed.Memo(xs)
  }
}

final class DirectOps[A](val xs: Direct[A]) extends AnyVal with ConversionOps[A] {
  def +:(y: A): Direct[A]          = Direct.join(Direct(y), xs)
  def :+(y: A): Direct[A]          = Direct.join(xs, Direct(y))
  def ++(ys: Direct[A]): Direct[A] = Direct.join(xs, ys)

  def apply(i: Index): A           = xs elemAt i
  def length: Int                  = xs.size.intSize
  def reverse: Direct[A]  = xs match {
    case Direct.Reversed(xs) => xs
    case _                   => new Direct.Reversed(xs)
  }
}
