package psp
package std
package ops

import api._, StdShow._, StdZero._

trait DirectViewOps[A, Repr] extends Any {
  def xs: DirectView[A, Repr]
  type MapTo = View[A] // XXX

  def distinct(implicit z: HashEq[A]): MapTo = xs withFilter xs.pset
  def sorted(implicit ord: Order[A]): MapTo = {
    val arr: Array[Object] = xs.castTo[View[Object]].toArray
    java.util.Arrays.sort(arr, ord.toScalaOrdering.castTo[Ordering[Object]])
    new DirectView(new Direct.WrapArray[A](arr))
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
  def reverseForeach(f: A => Unit): Unit = xs match {
    case xs: Direct[A] => xs.indices.reverse foreach (i => f(xs(i)))
    case _             => xs.pvec.reverse foreach f
  }

  def count(p: Predicate[A]): Int                        = foldl[Int](0)((res, x) => if (p(x)) res + 1 else res)
  def exists(p: Predicate[A]): Boolean                   = foldl[Boolean](false)((res, x) => if (p(x)) return true else res)
  def find(p: Predicate[A]): Option[A]                   = foldl[Option[A]](None)((res, x) => if (p(x)) return Some(x) else res)
  def first[B](pf: A ?=> B): Option[B]                   = find(pf.isDefinedAt) map pf
  def forall(p: Predicate[A]): Boolean                   = foldl[Boolean](true)((res, x) => if (!p(x)) return false else res)
  def head: A                                            = xs take      1 optionally { case Each(x) => x } orFail "empty.head"
  def indexWhere(p: Predicate[A]): Index                 = zipIndex(xs).find((x, i) => p(x)).fold(NoIndex)(_._2)
  def isEmpty: Boolean                                   = xs.size.isZero || directIsEmpty
  def last: A                                            = xs takeRight 1 optionally { case Each(x) => x } orFail "empty.last"
  def max(implicit ord: Order[A]): A                     = xs reducel (_ max2 _)
  def min(implicit ord: Order[A]): A                     = xs reducel (_ min2 _)
  def mkString(sep: String): String                      = stringed(sep)(_.any_s)
  def mk_s(sep: String)(implicit z: Show[A]): String     = stringed(sep)(_.to_s)
  def nonEmpty: Boolean                                  = xs.size.isNonZero || !directIsEmpty
  def tabular(columns: Shower[A]*): String               = if (xs.nonEmpty && columns.nonEmpty) FunctionGrid(xs.pvec, columns.m).render else ""
  def zfirst[B](pf: A ?=> B)(implicit z: Empty[B]): B    = find(pf.isDefinedAt).fold(z.empty)(pf)
  def zfoldl[B](f: (B, A) => B)(implicit z: Empty[B]): B = foldl(z.empty)(f)
  def zfoldr[B](f: (A, B) => B)(implicit z: Empty[B]): B = foldr(z.empty)(f)

  def distinct(implicit z: HashEq[A]): View[A]                   = xs.pset
  def distinctByEquals: View[A]                                  = distinct(HashEq.natural())
  def filter(p: Predicate[A]): View[A]                           = xs withFilter p
  def filterNot(p: Predicate[A]): View[A]                        = xs withFilter !p
  def gather[B](pf: A ?=> View[B]): View[B]                      = xs flatMap pf.zapply
  def grep(regex: Regex)(implicit z: Show[A]): View[A]           = xs filter (x => regex isMatch x)
  def init: View[A]                                              = xs dropRight 1
  def labelOp[B](label: String)(f: View[A] => View[B]): View[B]  = new LabeledView(f(xs), xs.viewOps :+ label)
  def mapApply[B, C](x: B)(implicit ev: A <:< (B => C)): View[C] = xs map (f => ev(f)(x))
  def mapWithIndex[B](f: (A, Index) => B): View[B]               = inView[B](mf => foldWithIndex(())((res, x, i) => mf(f(x, i))))
  def mapZip[B](f: A => B): View[(A, B)]                         = xs map (x => x -> f(x))
  def slice(range: IndexRange): View[A]                          = labelOp(pp"slice $range")(_ drop range.toDrop take range.toTake)
  def sortDistinct(implicit ord: Order[A]): View[A]              = new DirectApiViewOps(xs.pvec) sortDistinct
  def sorted(implicit ord: Order[A]): View[A]                    = new DirectApiViewOps(xs.pvec) sorted
  def tail: View[A]                                              = xs drop      1
  def toRefs: View[AnyRef]                                       = xs map (_.toRef)
  def withSize(size: Size): View[A]                              = new Each.Impl[A](size, xs foreach _)

  def foldWithIndex[B](zero: B)(f: (B, A, Index) => B): B = {
    var res = zero
    var index = Index(0)
    xs foreach { x =>
      res = f(res, x, index)
      index += 1
    }
    res
  }
  def foldl[B](zero: B)(f: (B, A) => B): B = {
    var res = zero
    xs foreach (x => res = f(res, x))
    res
  }
  def foldr[B](zero: B)(f: (A, B) => B): B = {
    var result = zero
    reverseForeach(x => result = f(x, result))
    result
  }

  /** TODO - possible map-creation methods.

  def ascendingFrequency: ExMap[A, Int]                     = unsortedFrequencyMap |> (_.orderByValue)
  def descendingFrequency: ExMap[A, Int]                    = ascendingFrequency.reverse
  def unsortedFrequencyMap: Map[A, Int]                     = sciMap(toScalaVector groupBy identity mapValues (_.size) toSeq: _*)
  def mapToAndOnto[B, C](k: A => B, v: A => C): ExMap[B, C] = toScalaVector |> (xs => newMap(xs map (x => k(x) -> v(x)): _*))
  def mapToMapPairs[B, C](f: A => (B, C)): ExMap[B, C]      = toScalaVector |> (xs => newMap(xs map f: _*))
  def groupBy[B, C](f: A => B)(g: Each[A] => C): ExMap[B, C] = {
    val buf = scmMap[B, pList[A]]() withDefaultValue newList[A]()
    pseq foreach (x => buf(f(x)) ::= x)
    newMap((buf.toMap mapValues g).toSeq: _*)
  }

  **/
}

trait InvariantViewOps[A] extends Any with ApiViewOps[A] {
  def +:(elem: A): View[A] = exView(elem) ++ xs
  def :+(elem: A): View[A] = xs ++ exView(elem)

  /** Can we figure out a way to abstract over these which pays off? */
  def contains(x: A)(implicit z: Eq[A]): Boolean = exists (_ === x)
  def containsRef(x: A with Object): Boolean     = exists (_ id_== x)
  def containsByEquals(x: A): Boolean            = exists (_ == x)
  def indexOf(x: A)(implicit z: Eq[A]): Index    = indexWhere (_ === x)
  def indexOfRef(x: A): Index                    = indexWhere (_ id_== x)
  def indexByEquals(x: A): Index                 = indexWhere (_ == x)
  def without(x: A)(implicit z: Eq[A]): View[A]  = xs filterNot (_ === x)
  def withoutRef(x: A): View[A]                  = xs filterNot (_ id_== x)
  def withoutByEquals(x: A): View[A]             = xs filterNot (_ == x)

  def findOr(p: Predicate[A], alt: => A): A           = find(p) | alt
  def product(implicit z: Products[A]): A             = xs.foldl(z.one)(z.product)
  def reducel(f: BinOp[A]): A                         = tail.foldl(head)(f)
  def sum(implicit z: Sums[A]): A                     = xs.foldl(z.zero)(z.sum)
  def zapply(i: Index)(implicit z: Empty[A]): A       = xs drop i.toSize zhead
  def zfind(p: Predicate[A])(implicit z: Empty[A]): A = findOr(p, z.empty)
  def zhead(implicit z: Empty[A]): A                  = if (isEmpty) z.empty else head
  def zlast(implicit z: Empty[A]): A                  = if (isEmpty) z.empty else last
  def zreduce(f: BinOp[A])(implicit z: Empty[A]): A   = if (isEmpty) z.empty else reducel(f)

  def mapOnto[B](f: A => B)(implicit z: HashEq[A]): ExMap[A, B] = xs.pset mapOnto f
  def zinit: View[A]                                            = if (isEmpty) emptyValue else init
  def ztail: View[A]                                            = if (isEmpty) emptyValue else tail
  def prepend(x: A): View[A]                                    = exView(x) ++ xs
  def append(x: A): View[A]                                     = xs ++ exView(x)

  def distinctBy[B: HashEq](f: A => B): View[A] = inView(mf =>
    zfoldl[ExSet[B]]((seen, x) => f(x) |> (y => try seen add y finally seen(y) || mf(x)))
  )

  def boundedClosure(maxDepth: Precise, f: A => View[A]): View[A] =
    if (maxDepth.isZero) xs else xs ++ (xs flatMap f).boundedClosure(maxDepth - 1, f)
}

final class DirectApiViewOps[A, Repr](val xs: DirectView[A, Repr]) extends AnyVal with DirectViewOps[A, Repr] {}
final class EachApiViewOps[A](val xs: View[A]) extends AnyVal with InvariantViewOps[A] { }

final class PairViewOps[R, A, B](val xs: View[R])(implicit paired: PairDown[R, A, B]) {
  // We should be able to write these method on the normal ViewOps and take the implicit
  // parameter here, like this:
  //   def mapPairs[B, C, D](f: (B, C) => D)(implicit z: Paired[A, B, C]): View[D] = xs map (x => f(z left x, z right x))
  // But scala's type inference sucks so it doesn't work without annotating the parameter types.
  def mapPairs[C](f: (A, B) => C): View[C]                                  = xs map (x => f(paired left x, paired right x))
  def mapLeft[R1, A1](f: A => A1)(implicit z: PairUp[R1, A1, B]): View[R1]  = xs map (x => z.create(f(paired left x), paired right x))
  def mapRight[R1, B1](f: B => B1)(implicit z: PairUp[R1, A, B1]): View[R1] = xs map (x => z.create(paired left x, f(paired right x)))
}
