package psp
package std
package ops

import api._, StdShow._

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

  def nonEmpty = !isEmpty
  def isEmpty: Boolean = { xs foreach (_ => return false) ; true }
  def foldl[B](zero: B)(f: (B, A) => B): B = {
    var res = zero
    xs foreach (x => res = f(res, x))
    res
  }
  def foldr[B](zero: B)(f: (A, B) => B): B = {
    var result = zero
    xs.pvec.reverse foreach(x => result = f(x, result))
    result
  }
  def force[That](implicit z: Builds[A, That]): That        = z build xs
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
  def toArray(implicit z: CTag[A]): Array[A] = Array.newBuilder[A] ++= toScalaTraversable result

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

trait CombinedOps[A] extends Any with ConversionOps[A] {
  def foldl[B](zero: B)(f: (B, A) => B): B

  private def stringed(sep: String)(f: A => String): String =
    foldl(new StringBuilder)((sb, x) => if (sb.isEmpty) sb append f(x) else sb append sep append f(x) ).result

  def mkString(sep: String): String                  = stringed(sep)(_.any_s)
  def mk_s(sep: String)(implicit z: Show[A]): String = stringed(sep)(_.to_s)

  def find(p: Predicate[A]): Option[A] = foldl[Option[A]](None)((res, x) => if (p(x)) return Some(x) else res)
  def forall(p: Predicate[A]): Boolean = foldl[Boolean](true)((res, x) => if (!p(x)) return false else res)
  def exists(p: Predicate[A]): Boolean = foldl[Boolean](false)((res, x) => if (p(x)) return true else res)
  def count(p: Predicate[A]): Int      = foldl[Int](0)((res, x) => if (p(x)) res + 1 else res)

  def indexAtWhich(p: Predicate[A]): Index = zipIndex(xs).find((x, i) => p(x)).fold(NoIndex)(_._2)
  def indexByEquals(x: A): Index           = indexAtWhich(_ == x)
  def containsByEquals(x: A): Boolean      = exists (_ == x)

  def contains(elem: A)(implicit z: Eq[A]): Boolean = exists (_ === elem)
  def containsRef(elem: A): Boolean                 = exists (_ id_== elem)

  final def collectFirst[B](pf: A ?=> B): Option[B]             = find(pf isDefinedAt _) map pf
  final def firstOrZero[B](pf: A ?=> B)(implicit z: Zero[B]): B = collectFirst(pf) | z.zero
  final def findOrZero(p: Predicate[A])(implicit z: Zero[A]): A = find(p) | z.zero

  def mapApply[B, C](x: B)(implicit ev: A <:< (B => C)): sciVector[C] = toScalaVector map (f => ev(f)(x))
  def mapOnto[B](f: A => B)(implicit z: HashEq[A]): exMap[A, B]       = xs.pset mapOnto f
  def mapFrom[B](f: A => B)(implicit z: HashEq[B]): exMap[B, A]       = xs map (x => f(x) -> x) pmap

  def findOr(p: Predicate[A], alt: => A): A            = find(p) | alt
  def sortDistinct(implicit ord: Order[A]): pVector[A] = toScalaVector.distinct sorted ord.toScalaOrdering
  def sortOrder[B: Order](f: A => B): pVector[A]       = toScalaVector sorted orderBy[A](f).toScalaOrdering
  def sorted(implicit ord: Order[A]): pVector[A]       = toScalaVector sorted ord.toScalaOrdering

  def foreachCounted(f: (Index, A) => Unit): Unit   = foldl(0.index)((idx, x) => try idx.next finally f(idx, x))

  def tabular(columns: (A => String)*): String = {
    val rows = pvec
    val cols = columns.m.pvec
    if (rows.isEmpty || cols.isEmpty) "" else FunctionGrid(rows, cols).render
  }

  def scanFilter(f: (sciSet[A], A) => Boolean): Foreach[A] =
    each(toScalaVector filter toScalaVector.foldLeft(sciSet[A]())((seen, x) => if (f(seen, x)) seen + x else seen))

  def distinctBy[B: Eq](f: A => B): Foreach[A] = scanFilter((res, x) => !(res exists (y => f(x) === f(y))))
  // def groupBy[B: HashEq](f: A => B): pMap[B, pSeq[A]]
  // def frequencyMap[B: HashEq](f: A => B): pMap[B, LongSize]
}

final class jIterableOps[A](val jxs: jIterable[A]) extends AnyVal with CombinedOps[A] {
  def xs = fromJava(jxs)
  override def isEmpty = !jxs.iterator.hasNext
}
final class sCollectionOps[A, CC[A] <: sCollection[A]](val sxs: CC[A]) extends AnyVal with CombinedOps[A] {
  def xs = fromScala(sxs)
  override def isEmpty = sxs.isEmpty
}
