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
  def xs: Foreach[A]

  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A]        = z build xs
  def toScala[CC[X]](implicit z: CanBuild[A, CC[A]]): CC[A] = to[CC](Builds wrap z)

  def toPolicyList: pList[A]                                            = PolicyList.builder[A] build xs
  def toPolicySeq: pSeq[A]                                              = Foreach.builder[A] build xs
  def toPolicySet(implicit z: HashEq[A]): exSet[A]                      = PolicySet.builder[A] build xs
  def toPolicyVector: pVector[A]                                        = Direct.builder[A] build xs
  def toPolicyMap[K: HashEq, V](implicit ev: A <:< (K, V)): exMap[K, V] = PolicyMap.builder[K, V] build (xs map ev)

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

  def biIterator: BiIterator[A] = biIterable.iterator
  def biIterable: BiIterable[A] = BiIterable(toScalaIterable)

  def plist: pList[A]                                                  = toPolicyList
  def pvec: pVector[A]                                                 = toPolicyVector
  def pseq: pSeq[A]                                                    = toPolicySeq
  def pset(implicit z: HashEq[A]): exSet[A]                            = toPolicySet
  def pmap[K, V](implicit ev: A <:< (K, V), z: HashEq[K]): exMap[K, V] = toPolicyMap[K, V]

  def naturalSet: exSet[A] = pset(HashEq.natural())
  def seq: sciSeq[A]       = toScalaSeq // varargs
}

final class ForeachOps[A](val xs: Foreach[A]) extends AnyVal with ConversionOps[A]

final class DirectOps[A](val xs: Direct[A]) extends AnyVal with ConversionOps[A] {
  def apply(i: Index): A = xs elemAt i
  def length: Int        = xs.size.intSize
  def reverse: Direct[A]  = xs match {
    case Direct.Reversed(xs) => xs
    case _                   => new Direct.Reversed(xs)
  }
}

