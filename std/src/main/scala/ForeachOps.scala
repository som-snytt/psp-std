package psp
package std
package ops

import api._

final class ArraySpecificOps[A](val xs: Array[A]) extends AnyVal with HasPreciseSizeMethods {
  def size: IntSize = Precise(xs.length)
  private def andThis(op: Unit): xs.type = xs

  def apply(idx: Index): A                   = xs(idx.safeInt)
  def updated(idx: Index, value: A): xs.type = andThis(xs(idx.safeInt) = value)
  def mapInPlace(f: A => A): xs.type         = andThis(foreachIndex(i => updated(i, f(apply(i)))))
  def sortInPlace: Array[A]                  = Array sortInPlace xs
}

trait ConversionOps[A] extends Any {
  def xs: Each[A]

  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A]        = z build xs
  def toScala[CC[X]](implicit z: CanBuild[A, CC[A]]): CC[A] = to[CC](Builds wrap z)

  def toEach: Each[A]                                                     = xs
  def toLinear: Linear[A]                                                 = xs match { case xs: Linear[A] => xs   ; case _ => Linear.builder[A] build xs            }
  def toDirect: Direct[A]                                                 = xs match { case xs: Direct[A] => xs   ; case _ => Direct.builder[A] build xs            }
  def toExSet(implicit z: HashEq[A]): ExSet[A]                            = xs match { case xs: ExSet[A] => xs    ; case _ => ExSet.builder[A] build xs             }
  def toExMap[K, V](implicit ev: A <:< (K, V), z: HashEq[K]): ExMap[K, V] = xs match { case xs: ExMap[K, V] => xs ; case _ => ExMap.builder[K, V] build (xs map ev) }

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

  def iterator: scIterator[A] = BiIterator(xs)
  def trav: scTraversable[A]  = toScalaTraversable
  def seq: sciSeq[A]          = toScalaSeq // varargs
}

final class ForeachOps[A](val xs: Each[A]) extends AnyVal with ConversionOps[A] {
  def sized(size: Precise): Each[A] = new Each.Sized(xs, size)
  def memo: Indexed.Memo[A] = xs match {
    case xs: Indexed.Memo[A] => xs
    case _                   => new Indexed.Memo(xs)
  }
}

final class DirectOps[A](val xs: Direct[A]) extends AnyVal with ConversionOps[A] {
  def +:(x: A): Direct[A]          = Direct.prepend(x, xs)
  def :+(x: A): Direct[A]          = Direct.append(xs, x)
  def ++(ys: Direct[A]): Direct[A] = Direct.join(xs, ys)

  def apply(i: Index): A           = xs elemAt i
  def length: Int                  = xs.size.intSize
  def reverse: Direct[A]  = xs match {
    case Direct.Reversed(xs) => xs
    case _                   => new Direct.Reversed(xs)
  }
}

final class LinearOps[A](val xs: Linear[A]) extends AnyVal with ConversionOps[A] {
  def ::(x: A): Linear[A] = Linear.cons(x, xs)
}
