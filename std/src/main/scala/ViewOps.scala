package psp
package std
package ops

import api._, StdShow._, StdZero._

trait DirectViewOps[A, Repr] extends Any {
  def xs: DirectView[A, Repr]
  type MapTo = View[A] // XXX

  def distinct(implicit z: HashEq[A]): MapTo = xs withFilter xs.toExSet
  def sorted(implicit ord: Order[A]): MapTo = {
    val arr: Array[Object] = xs.castTo[View[Object]].toArray
    java.util.Arrays.sort(arr, ord.toScalaOrdering.castTo[Ordering[Object]])
    new DirectView(Direct wrapArray[A] arr)
  }
  def sortByShow(implicit z: Show[A]): MapTo              = sortBy(_.to_s)
  def sortBy[B](f: A => B)(implicit ord: Order[B]): MapTo = sorted(orderBy[A](f))
  def sortDistinct(implicit ord: Order[A]): MapTo         = sorted distinct ord.toHashEq
}

trait ApiViewOps[+A] extends Any {
  def xs: View[A]

  private def stringed(sep: String)(f: Shower[A]): String = xs map f zreduce (_ ~ sep ~ _)
  private def directIsEmpty: Boolean = {
    xs foreach (_ => return false)
    true
  }

  def foreachWithIndex(f: (A, Index) => Unit): Unit = foldl(0.index)((idx, x) => try idx.next finally f(x, idx))
  def foreachReverse(f: A => Unit): Unit            = xs.toDirect |> (xs => xs.indices foreachReverse (i => f(xs(i))))

  def count(p: Predicate[A]): Int                        = foldl[Int](0)((res, x) => if (p(x)) res + 1 else res)
  def exists(p: Predicate[A]): Boolean                   = foldl[Boolean](false)((res, x) => if (p(x)) return true else res)
  def find(p: Predicate[A]): Option[A]                   = foldl[Option[A]](None)((res, x) => if (p(x)) return Some(x) else res)
  def first[B](pf: A ?=> B): Option[B]                   = find(pf.isDefinedAt) map pf
  def forallTrue(implicit ev: A <:< Boolean): Boolean    = forall(x => ev(x))
  def forall(p: Predicate[A]): Boolean                   = foldl[Boolean](true)((res, x) => if (!p(x)) return false else res)
  def head: A                                            = xs take 1 optionally { case Each(x) => x } orFail "empty.head"
  def indexWhere(p: Predicate[A]): Index                 = zipIndex findLeft p map snd or NoIndex
  def indicesWhere(p: Predicate[A]): View[Index]         = zipIndex filterLeft p map snd
  def isEmpty: Boolean                                   = xs.size.isZero || directIsEmpty
  def last: A                                            = xs takeRight 1 optionally { case Each(x) => x } orFail "empty.last"
  def max(implicit ord: Order[A]): A                     = xs reducel (_ max2 _)
  def min(implicit ord: Order[A]): A                     = xs reducel (_ min2 _)
  def mkString(sep: String): String                      = stringed(sep)(_.any_s)
  def mk_s(sep: String)(implicit z: Show[A]): String     = stringed(sep)(_.to_s)
  def nonEmpty: Boolean                                  = xs.size.isNonZero || !directIsEmpty
  def tabular(columns: Shower[A]*): String               = if (xs.nonEmpty && columns.nonEmpty) FunctionGrid(xs.toDirect, columns.m).render else ""
  def zfirst[B](pf: A ?=> B)(implicit z: Empty[B]): B    = find(pf.isDefinedAt).fold(z.empty)(pf)
  def zfoldl[B](f: (B, A) => B)(implicit z: Empty[B]): B = foldl(z.empty)(f)
  def zfoldr[B](f: (A, B) => B)(implicit z: Empty[B]): B = foldr(z.empty)(f)

  def filter(p: Predicate[A]): View[A]                           = xs withFilter p
  def filterNot(p: Predicate[A]): View[A]                        = xs withFilter !p
  def gather[B](pf: A ?=> View[B]): View[B]                      = xs flatMap pf.zapply
  def gatherClass[B: CTag] : View[B]                             = xs collect classFilter[B]
  def grep(regex: Regex)(implicit z: Show[A]): View[A]           = xs filter (x => regex isMatch x)
  def init: View[A]                                              = xs dropRight 1
  def labelOp[B](label: String)(f: View[A] => View[B]): View[B]  = new LabeledView(f(xs), xs.viewOps :+ label)
  def mapApply[B, C](x: B)(implicit ev: A <:< (B => C)): View[C] = xs map (f => ev(f)(x))
  def mapWithIndex[B](f: (A, Index) => B): View[B]               = inView[B](mf => foldWithIndex(())((res, x, i) => mf(f(x, i))))
  def mapZip[B](f: A => B): View[(A, B)]                         = xs map (x => x -> f(x))
  def slice(range: IndexRange): View[A]                          = labelOp(pp"slice $range")(_ drop range.toDrop take range.toTake)
  def sliceWhile(p: Predicate[A], q: Predicate[A]): View[A]      = xs dropWhile p takeWhile q
  def sortDistinct(implicit ord: Order[A]): View[A]              = new DirectApiViewOps(xs.toDirect) sortDistinct
  def sorted(implicit ord: Order[A]): View[A]                    = new DirectApiViewOps(xs.toDirect) sorted
  def tail: View[A]                                              = xs drop 1
  def toRefs: View[Ref[A]]                                       = xs map (_.toRef)
  def withSize(size: Size): View[A]                              = new Each.Impl[A](size, xs foreach _)
  def zip[B](ys: View[B]): ZipView[A, B]                         = new ZipView(xs, ys)
  def zipIndex: ZipView[A, Index]                                = new ZipView(xs, Each.indices)

  def ofClass[B: CTag] : View[B]            = xs collect classFilter[B]
  def takeToFirst(p: Predicate[A]): View[A] = xs span !p mapRight (_ take 1) rejoin
  def dropIndex(index: Index): View[A]      = xs splitAt index mapRight (_ drop 1) rejoin

  def groupBy[B: HashEq](f: A => B): ExMap[B, View[A]] =
    foldl(bufferMap[B, View[A]]())((buf, x) => andResult(buf, buf(f(x)) :+= x)).toMap.m.toExMap

  def foldWithIndex[B](zero: B)(f: (B, A, Index) => B): B = foldFrom(zero) indexed f
  def foldl[B](zero: B)(f: (B, A) => B): B                = foldFrom(zero) left f
  def foldr[B](zero: B)(f: (A, B) => B): B                = foldFrom(zero) right f

  def foldFrom[B](zero: B): FoldOps[A, B]          = FoldOpsClass(xs, zero)
  def fold[B](implicit z: Empty[B]): FoldOps[A, B] = FoldOpsClass(xs, z.empty)
}

trait ExtensionalOps[A] extends Any {
  protected def xs: View[A]
  protected implicit def eqs[A]: HashEq[A]

  def contains(x: A): Boolean            = xs contains x
  def distinct: View[A]                  = toSet
  def indexOf(x: A): Index               = xs indexOf x
  def indicesOf(x: A): View[Index]       = xs indicesOf x
  def mapOnto[B](f: A => B): ExMap[A, B] = toSet mapOnto f
  def toBag: Bag[A]                      = xs.toBag
  def toSet: ExSet[A]                    = xs.toExSet
  def without(x: A): View[A]             = xs without x

  def toMap[K, V](implicit ev: A <:< (K, V)): ExMap[K, V] = xs.toExMap
}
final class ByEqualsExtensionalOps[A](val xs: View[A]) extends AnyVal with ExtensionalOps[A] {
  protected def eqs[A]: HashEq[A] = HashEq.natural[A]()
}
final class ByRefExtensionalOps[A <: AnyRef](val xs: View[A]) extends AnyVal with ExtensionalOps[A] {
  protected def eqs[A]: HashEq[A] = HashEq(_.toRef eq _.toRef, identityHashCode)
}

trait InvariantViewOps[A] extends Any with ApiViewOps[A] {
  def +:(elem: A): View[A] = exView(elem) ++ xs
  def :+(elem: A): View[A] = xs ++ exView(elem)

  def byEquals: ExtensionalOps[A]          = new ByEqualsExtensionalOps[A](xs)
  def byRef: ExtensionalOps[A with Object] = new ByRefExtensionalOps[A with Object](toRefs)

  def contains(x: A)(implicit z: Eq[A]): Boolean                = exists (_ === x)
  def distinct(implicit z: HashEq[A]): View[A]                  = xs.toExSet
  def indexOf(x: A)(implicit z: Eq[A]): Index                   = indexWhere (_ === x)
  def indicesOf(x: A)(implicit z: Eq[A]): View[Index]           = indicesWhere (_ === x)
  def mapOnto[B](f: A => B)(implicit z: HashEq[A]): ExMap[A, B] = xs.toExSet mapOnto f
  def toBag(implicit z: HashEq[A]): Bag[A]                      = exMap((xs.toScalaVector groupBy identity mapValues (_.size.size: Precise)).toSeq: _*)
  def without(x: A)(implicit z: Eq[A]): View[A]                 = xs filterNot (_ === x)
  def withoutEmpty(implicit z: Empty[A], eqs: Eq[A]): View[A]   = xs without z.empty

  def findOr(p: Predicate[A], alt: => A): A           = find(p) | alt
  def product(implicit z: Products[A]): A             = xs.foldl(z.one)(z.product)
  def reducel(f: BinOp[A]): A                         = tail.foldl(head)(f)
  def sum(implicit z: Sums[A]): A                     = xs.foldl(z.zero)(z.sum)
  def zapply(i: Index)(implicit z: Empty[A]): A       = xs drop i.sizeExcluding zhead
  def zfind(p: Predicate[A])(implicit z: Empty[A]): A = findOr(p, z.empty)
  def zhead(implicit z: Empty[A]): A                  = if (isEmpty) z.empty else head
  def zlast(implicit z: Empty[A]): A                  = if (isEmpty) z.empty else last
  def zreduce(f: BinOp[A])(implicit z: Empty[A]): A   = if (isEmpty) z.empty else reducel(f)

  def zinit: View[A]                    = if (isEmpty) emptyValue[View[A]] else init
  def ztail: View[A]                    = if (isEmpty) emptyValue[View[A]] else tail
  def prepend(x: A): View[A]            = exView(x) ++ xs
  def append(x: A): View[A]             = xs ++ exView(x)
  def intersperse(ys: View[A]): View[A] = xs zip ys flatMap ((x, y) => Direct(x, y))

  def transpose[B](implicit ev: A <:< View[B]): View[View[B]] = {
    val grid = xs map ev
    def col(n: Index) = grid map (_ drop n.sizeExcluding head)
    Each.indices map col
  }

  def mpartition(p: View[A] => Predicate[A]): View[View[A]] =
    inView[View[A]](mf => xs partition p(xs.memo) match { case Split(xs, ys) => mf(xs) ; ys mpartition p foreach mf })

  def distinctBy[B: HashEq](f: A => B): View[A] = inView(mf =>
    zfoldl[ExSet[B]]((seen, x) => f(x) |> (y => try seen add y finally seen(y) || mf(x)))
  )

  def grouped(n: Precise): View[View[A]] = new Grouped[A](n) apply xs

  private def iteratively[B](xs: View[A], head: View[A] => B, tail: View[A] => View[A]): View[B] = xs match {
    case _ if xs.isEmpty => emptyValue
    case _               => head(xs) +: iteratively(tail(xs), head, tail)
  }

  def boundedClosure(maxDepth: Precise, f: A => View[A]): View[A] =
    if (maxDepth.isZero) xs else xs ++ (xs flatMap f).boundedClosure(maxDepth - 1, f)
}

private abstract class HeadAndTail[A, B] {
  def isDone(xs: View[A]): Boolean
  def head(xs: View[A]): B
  def tail(xs: View[A]): View[A]
  def apply(xs: View[A]): View[B] = if (isDone(xs)) emptyValue else head(xs) +: apply(tail(xs)) // XXX @tailrec
}
private class Grouped[A](n: Precise) extends HeadAndTail[A, View[A]] {
  def isDone(xs: View[A]): Boolean = xs.isEmpty
  def head(xs: View[A]): View[A]   = xs take n
  def tail(xs: View[A]): View[A]   = xs drop n
}

final class DirectApiViewOps[A, Repr](val xs: DirectView[A, Repr]) extends AnyVal with DirectViewOps[A, Repr] {}
final class EachApiViewOps[A](val xs: View[A]) extends AnyVal with InvariantViewOps[A] { }
final class InvariantApiViewOps[A](val xs: InvariantView[A]) extends AnyVal with InvariantViewOps[A] { }

final class PairViewOps[R, A, B](val xs: View[R])(implicit paired: PairDown[R, A, B]) {
  // We should be able to write these method on the normal ViewOps and take the implicit
  // parameter here, like this:
  //   def mapPairs[B, C, D](f: (B, C) => D)(implicit z: Paired[A, B, C]): View[D] = xs map (x => f(z left x, z right x))
  // But scala's type inference sucks so it doesn't work without annotating the parameter types.
  def mapPairs[C](f: (A, B) => C): View[C]                                  = xs map (x => f(paired left x, paired right x))
  def mapLeft[R1, A1](f: A => A1)(implicit z: PairUp[R1, A1, B]): View[R1]  = xs map (x => z.create(f(paired left x), paired right x))
  def mapRight[R1, B1](f: B => B1)(implicit z: PairUp[R1, A, B1]): View[R1] = xs map (x => z.create(paired left x, f(paired right x)))
}
