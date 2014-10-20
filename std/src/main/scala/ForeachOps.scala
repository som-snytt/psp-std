package psp
package std
package ops

import api._, StdShow._

final class ArraySpecificOps[A](val xs: Array[A]) extends AnyVal with HasPreciseSizeMethods {
  def size: PreciseSize = sizeInfo
  def sizeInfo = newSize(xs.length)
  private def andThis(op: Unit): xs.type = xs

  def apply(idx: Index): A                   = xs(idx.safeToInt)
  def updated(idx: Index, value: A): xs.type = andThis(xs(idx.safeToInt) = value)
  def mapInPlace(f: A => A): xs.type         = andThis(foreachIndex(i => updated(i, f(apply(i)))))
  def sortInPlace: Array[A]                  = Array sortInPlace xs
}

trait ConversionOps[A] extends Any {
  protected def underlying: Foreach[A]

  def nonEmpty = !isEmpty
  def isEmpty: Boolean = { runForeach(_ => return false) ; true }
  def runForeach(f: A => Unit): Unit
  def foldl[B](zero: B)(f: (B, A) => B): B = {
    var res = zero
    runForeach(x => res = f(res, x))
    res
  }
  def foldr[B](zero: B)(f: (A, B) => B): B = {
    var result = zero
    underlying.pvec.reverse foreach(x => result = f(x, result))
    result
  }
  def force[That](implicit z: Builds[A, That]): That        = z(runForeach)
  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A]        = z(runForeach)
  def toScala[CC[X]](implicit z: CanBuild[A, CC[A]]): CC[A] = to[CC](Builds wrap z)

  def mapWithIndex[B](f: (A, Index) => B): pVector[B] = ( for ((x, i) <- toScalaVector.zipWithIndex) yield f(x, Index(i)) ).pvec
  def mapWithNth[B](f: (A, Nth) => B): pVector[B]     = ( for ((x, i) <- toScalaVector.zipWithIndex) yield f(x, Nth(i + 1)) ).pvec

  def pmap[K, V](implicit ev: A <:< (K, V), z: HashEq[K]): pMap[K, V]        = toPolicyMap[K, V]
  def toPolicyMap[K, V](implicit ev: A <:< (K, V), z: HashEq[K]): pMap[K, V] = PolicyMap.builder[K, V] build (underlying map ev)

  def toPolicySet(implicit z: HashEq[A]): exSet[A] = PolicySet.builder[A] build underlying
  def toPolicyList: pList[A]                       = PolicyList.builder[A] build underlying
  def toPolicyVector: pVector[A]                   = Direct.builder[A] build underlying
  def toPolicySeq: pSeq[A]                         = Foreach.builder[A] build underlying

  def toScalaIterable: scIterable[A]                            = toScala[scIterable]
  def toScalaList: sList[A]                                     = toScala[sciList]
  def toScalaSet: sSet[A]                                       = toScala[sciSet]
  def toScalaVector: sVector[A]                                 = toScala[sciVector]
  def toScalaSeq: sSeq[A]                                       = toScala[sciSeq]
  def toScalaStream: sciStream[A]                               = toScala[sciStream]
  def toScalaTraversable: scTraversable[A]                      = toScala[scTraversable]
  def toScalaMap[K, V](implicit ev: A <:< (K, V)): sciMap[K, V] = toScalaVector map ev toMap

  def toArray(implicit z: CTag[A]): Array[A] = Array.newBuilder[A] ++= trav result
  def toJava: jList[A]                       = jList(seq: _*)
  def toJavaSet: jSet[A]                     = jSet(seq: _*)

  def biIterator: BiIterator[A] = biIterable.iterator
  def biIterable: BiIterable[A] = BiIterable(toScalaIterable)

  def plist: pList[A]                      = toPolicyList
  def pvec: pVector[A]                     = toPolicyVector
  def pseq: pSeq[A]                        = toPolicySeq
  def pset(implicit z: HashEq[A]): pSet[A] = toPolicySet
  def naturalSet: pSet[A]                  = pset(HashEq.natural())

  def seq: sciSeq[A] = toScala[sciSeq] // varargs

  // private def xs: Foreach[A]       = Foreach(runForeach)
  private def trav: scTraversable[A] = toScalaTraversable
}

trait CombinedOps[A] extends Any with ConversionOps[A] {
  def foldl[B](zero: B)(f: (B, A) => B): B

  private def stringed(sep: String)(f: A => String): String =
    foldl(new StringBuilder)((sb, x) => if (sb.isEmpty) sb append f(x) else sb append sep append f(x) ).result

  def mkString(sep: String): String                      = stringed(sep)(_.any_s)
  def find(p: Predicate[A]): Option[A]                   = foldl[Option[A]](None)((res, x) => if (p(x)) return Some(x) else res)
  def forall(p: Predicate[A]): Boolean                   = foldl[Boolean](true)((res, x) => if (!p(x)) return false else res)
  def exists(p: Predicate[A]): Boolean                   = foldl[Boolean](false)((res, x) => if (p(x)) return true else res)
  def count(p: A => Boolean): Int                        = foldl[Int](0)((res, x) => if (p(x)) res + 1 else res)

  def indexAtWhich(p: A => Boolean): Index = zipIndex(Foreach(runForeach)).find((x, i) => p(x)).fold(NoIndex)(_._2)
  def indexByEquals(x: A): Index           = indexAtWhich(_ == x)
  def containsByEquals(x: A): Boolean      = exists (_ == x)

  def contains(elem: A)(implicit z: Eq[A]): Boolean = exists (_ === elem)
  def containsRef(elem: A): Boolean                 = exists (_ id_== elem)

  final def collectFirst[B](pf: A ?=> B): Option[B]             = find(pf isDefinedAt _) map pf
  final def firstOrZero[B](pf: A ?=> B)(implicit z: Zero[B]): B = collectFirst(pf) | z.zero
  final def findOrZero(p: A => Boolean)(implicit z: Zero[A]): A = find(p) | z.zero

  def mapApply[B, C](x: B)(implicit ev: A <:< (B => C)): sciVector[C] = toScalaVector map (f => ev(f)(x))
  def mapOnto[B](f: A => B)(implicit z: HashEq[A]): pMap[A, B]        = newMap(underlying.pvec map (x => x -> f(x)))
  def mapFrom[B](f: A => B)(implicit z: HashEq[B]): pMap[B, A]        = newMap(underlying.pvec map (x => f(x) -> x))

  def findOr(p: A => Boolean, alt: => A): A            = find(p) | alt
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
  // def frequencyMap[B: HashEq](f: A => B): pMap[B, PreciseSize]
}

final class jIterableOps[A](val xs: jIterable[A]) extends AnyVal with CombinedOps[A] {
  protected def underlying: Foreach[A] = fromJava(xs)
  override def isEmpty = !xs.iterator.hasNext
  def runForeach(f: A => Unit): Unit = BiIterable(xs) foreach f
}
final class sCollectionOps[A, CC[A] <: sCollection[A]](val xs: CC[A]) extends AnyVal with CombinedOps[A] {
  protected def underlying: Foreach[A] = fromScala(xs)
  override def isEmpty = xs.isEmpty
  def build(implicit z: Builds[A, CC[A]]): CC[A] = force[CC[A]]
  def runForeach(f: A => Unit): Unit = xs foreach f
}

case class Coordinate[A](row: Nth, column: Nth, value: A, to_s: String) {
  def width = newSize(to_s.length)
  def fmt   = width.leftFormatString
  override def toString = to_s
}
