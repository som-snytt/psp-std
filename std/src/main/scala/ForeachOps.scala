package psp
package std
package ops

import api._
import linear._

final class ArraySpecificOps[A](val xs: Array[A]) extends AnyVal {
  private def andThis(op: Unit): xs.type = xs

  def updated(idx: Index, value: A) = andThis(xs(idx.indexValue) = value)
  def mapInPlace(f: A => A)         = andThis(xs.size foreachIndex (i => xs(i.indexValue) = f(xs(i.indexValue))))
  def sortInPlace: Array[A]         = Array sortInPlace xs
}

trait ConversionOps[A] extends Any {
  def nonEmpty = !isEmpty
  def isEmpty: Boolean = { runForeach(_ => return false) ; true }
  def runForeach(f: A => Unit): Unit
  def foldl[B](zero: B)(f: (B, A) => B): B = {
    var res = zero
    runForeach(x => res = f(res, x))
    res
  }
  def force[That](implicit z: Builds[A, That]): That        = z(runForeach)
  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A]        = z(runForeach)
  def toScala[CC[X]](implicit z: CanBuild[A, CC[A]]): CC[A] = to[CC](Builds wrap z)

  def mapWithIndex[B](f: (Index, A) => B): pSeq[B] = ( for ((x, i) <- toScalaVector.zipWithIndex) yield f(i.index, x) ).toPolicySeq
  def mapWithNth[B](f: (Nth, A) => B): pSeq[B]     = ( for ((x, i) <- toScalaVector.zipWithIndex) yield f((i + 1).nth, x) ).toPolicySeq

  def toPolicySet(implicit z: HashEq[A]): pSet[A]              = PolicySet.builder[A].apply(runForeach)
  def toPolicyMap[K, V](implicit ev: A <:< (K, V)): pMap[K, V] = PolicyMap.builder[K, V] build (Foreach(runForeach) map ev)
  def toPolicyList: pList[A]                                   = PolicyList.builder[A](runForeach)
  def toPolicyVector: pVector[A]                               = Direct.builder[A](runForeach)
  def toPolicySeq: pSeq[A]                                     = Foreach.builder[A](runForeach)
  def toPolicyStream                                           = foldl(Stream.empty[A])((x, y) => Stream.cons(y, x))

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

  def biIterator: BiIterator[A] = biIterable.iterator
  def biIterable: BiIterable[A] = BiIterable(toScalaIterable)

  def pvec: pVector[A]                                  = toPolicyVector
  def pseq: pSeq[A]                                     = toPolicySeq
  def pset(implicit z: HashEq[A]): pSet[A]              = toPolicySet
  def pmap[K, V](implicit ev: A <:< (K, V)): pMap[K, V] = toPolicyMap[K, V]
  def naturalSet: pSet[A]                               = pset(HashEq.natural())

  def seq: sciSeq[A] = toScala[sciSeq] // varargs

  // private def xs: Foreach[A]       = Foreach(runForeach)
  private def trav: scTraversable[A] = toScalaTraversable
}

trait CombinedOps[A] extends Any with ConversionOps[A] {
  def foldl[B](zero: B)(f: (B, A) => B): B

  private def stringed(sep: String)(f: A => String): String =
    foldl(new StringBuilder)((sb, x) => if (sb.isEmpty) sb append f(x) else sb append sep append f(x) ).result

  def joinEOL(implicit z: TryShow[A]): String            = stringed(EOL)(_.try_s)
  def join(sep: String)(implicit shows: Show[A]): String = stringed(sep)(_.to_s)
  // def joinLines(implicit shows: Show[A]): String         = join(EOL)
  // def joinComma(implicit shows: Show[A]): String         = join(", ")
  def joinSpace(implicit shows: Show[A]): String         = join(" ")
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

  def ascendingFrequency: pMap[A, Int]                         = unsortedFrequencyMap |> (_.orderByValue)
  def descendingFrequency: pMap[A, Int]                        = ascendingFrequency.reverse
  def findOr(p: A => Boolean, alt: => A): A                    = find(p) | alt
  def mapFrom[B](f: A => B): pMap[B, A]                        = newMap(toScalaVector map (x => f(x) -> x): _*)
  def mapOnto[B](f: A => B): pMap[A, B]                        = newMap(toScalaVector map (x => x -> f(x)): _*)
  def mapToAndOnto[B, C](k: A => B, v: A => C): pMap[B, C]     = toScalaVector |> (xs => newMap(xs map (x => k(x) -> v(x)): _*))
  def mapToMapPairs[B, C](f: A => (B, C)): pMap[B, C]          = toScalaVector |> (xs => newMap(xs map f: _*))
  def sortDistinct(implicit ord: Order[A]): pVector[A]         = toScalaVector.distinct sorted ord.toScalaOrdering
  def sortByShow(implicit z: Show[A]): pVector[A]              = toScalaVector sorted orderBy[A](_.to_s).toScalaOrdering
  def sortOrder[B: Order](f: A => B): pVector[A]               = toScalaVector sorted orderBy[A](f).toScalaOrdering
  def sorted(implicit ord: Order[A]): pVector[A]               = toScalaVector sorted ord.toScalaOrdering
  def unsortedFrequencyMap: Map[A, Int]                        = sciMap(toScalaVector groupBy identity mapValues (_.size) toSeq: _*)
  def sameMembers(ys: sCollection[A])(implicit eqs: HashEq[A]) = toPolicySet === each(ys).toPolicySet
  // def sameLength(that: sCollection[_])                      = toScalaVector hasSameSize that

  def foreachCounted(f: (Index, A) => Unit): Unit   = foldl(0.index)((idx, x) => try idx.next finally f(idx, x))

  def groupBy[B, C](f: A => B)(g: pSeq[A] => C): pMap[B, C] = {
    val buf = scmMap[B, pList[A]]() withDefaultValue newList[A]()
    pseq foreach (x => buf(f(x)) ::= x)
    newMap((buf.toMap mapValues g).toSeq: _*)
  }

  def tabular(columns: (A => String)*): String = tabularLines(columns: _*) mkString EOL
  def tabularLines(columns: (A => String)*): pVector[String] = {
    val rows   = pvec
    val cols   = columns.m.pvec
    if (rows.isEmpty || cols.isEmpty) return newVector()

    val widths: pVector[Int] = cols map (f => rows map f map (_.length) max)

    def one(width: Int, value: String): String = if (width == 0 || value == "") "" else s"%-${width}s" format value

    // In 2.11, compiles like you'd expect:
    //   def oneRow(base: A): String = (widths, cols map (_ apply base)) map one mkString " "
    // Handholding necessary to compile in 2.10.
    // def oneRow(base: A): String = new Tuple2WalkableOps[Direct[Int], Int, Direct[String], String](widths -> (cols map (_ apply base))) map one mkString " "
    // def oneRow(base: A): String = new Tuple2WalkableOps(widths -> (cols map (_ apply base))) map one mkString " "
    def oneRow(base: A): String = zip2(widths, cols map (_ apply base)) map one mkString " "

    rows map oneRow
  }

  def scanFilter(f: (sciSet[A], A) => Boolean): Foreach[A] =
    each(toScalaVector filter toScalaVector.foldLeft(sciSet[A]())((seen, x) => if (f(seen, x)) seen + x else seen))

  def distinctBy[B: Eq](f: A => B): Foreach[A] = scanFilter((res, x) => !(res exists (y => f(x) === f(y))))
}

final class jIterableOps[A](val xs: jIterable[A]) extends AnyVal with CombinedOps[A] {
  override def isEmpty = !xs.iterator.hasNext
  def runForeach(f: A => Unit): Unit = BiIterable(xs) foreach f
}
final class sCollectionOps[A, CC[A] <: sCollection[A]](val xs: CC[A]) extends AnyVal with CombinedOps[A] {
  override def isEmpty = xs.isEmpty
  def build(implicit z: Builds[A, CC[A]]): CC[A] = force[CC[A]]
  def runForeach(f: A => Unit): Unit = xs foreach f
}
