package psp
package std
package ops

import api._, StdShow._, StdZero._

trait ApiViewOps[+A] extends Any {
  def xs: View[A]

  private def stringed(sep: String)(f: Shower[A]): String = xs map f safeReduce (_ ~ sep ~ _)
  private def directIsEmpty: Boolean = {
    xs foreach (_ => return false)
    true
  }

  def foreachWithIndex(f: (A, Index) => Unit): Unit = foldl(0.index)((idx, x) => try idx.next finally f(x, idx))
  def mapWithIndex[B](f: (A, Index) => B): View[B]  = inView[B](mf => foreachWithIndex(f andThen mf))
  def indexAtWhich(p: Predicate[A]): Index          = zipIndex(xs).find((x, i) => p(x)).fold(NoIndex)(_._2)

  def isEmpty: Boolean     = xs.size.isZero || directIsEmpty
  def nonEmpty: Boolean    = xs.size.isNonZero || !directIsEmpty
  def head: A              = xs take      1 optionally { case PSeq(x) => x } orFail "empty.head"
  def init: View[A]        = xs dropRight 1
  def last: A              = xs takeRight 1 optionally { case PSeq(x) => x } orFail "empty.last"
  def tail: View[A]        = xs drop      1
  def toRefs: View[AnyRef] = xs map (_.toRef)

  def slice(range: IndexRange): View[A]                = new LabeledView(xs drop range.precedingSize take range.size, xs.viewOps :+ pp"slice $range")
  def filter(p: Predicate[A]): View[A]                 = xs withFilter p
  def filterNot(p: Predicate[A]): View[A]              = xs withFilter !p
  def distinct(implicit z: HashEq[A]): View[A]         = xs.pset
  def max(implicit ord: Order[A]): A                   = xs reducel (_ max2 _)
  def min(implicit ord: Order[A]): A                   = xs reducel (_ min2 _)
  def sortDistinct(implicit ord: Order[A]): Direct[A] = xs.toScalaVector.distinct sorted ord.toScalaOrdering
  def sorted(implicit ord: Order[A]): Direct[A]       = xs.toScalaVector sorted ord.toScalaOrdering

  def count(p: Predicate[A]): Int      = foldl[Int](0)((res, x) => if (p(x)) res + 1 else res)
  def exists(p: Predicate[A]): Boolean = foldl[Boolean](false)((res, x) => if (p(x)) return true else res)
  def find(p: Predicate[A]): Option[A] = foldl[Option[A]](None)((res, x) => if (p(x)) return Some(x) else res)
  def forall(p: Predicate[A]): Boolean = foldl[Boolean](true)((res, x) => if (!p(x)) return false else res)

  def collectFirst[B](pf: A ?=> B): Option[B]                    = find(pf.isDefinedAt) map pf
  def firstOrZero[B](pf: A ?=> B)(implicit z: Zero[B]): B        = find(pf.isDefinedAt).fold(z.zero)(pf)
  def flatCollect[B](pf: A ?=> View[B]): View[B]                 = xs flatMap pf.applyOrZero
  def grep(regex: Regex)(implicit z: Show[A]): View[A]           = xs filter (x => regex isMatch x)
  def mapApply[B, C](x: B)(implicit ev: A <:< (B => C)): View[C] = xs map (f => ev(f)(x))
  def mapZip[B](f: A => B): View[(A, B)]                         = xs map (x => x -> f(x))
  def withSize(size: Size): View[A]                              = new Each.Impl[A](size, xs foreach _)
  def mkString(sep: String): String                              = stringed(sep)(_.any_s)
  def mk_s(sep: String)(implicit z: Show[A]): String             = stringed(sep)(_.to_s)
  def sortOrder[B: Order](f: A => B): Direct[A]                 = sorted(orderBy[A](f))
  def tabular(columns: Shower[A]*): String                       = if (xs.nonEmpty && columns.nonEmpty) FunctionGrid(xs.pvec, columns.m).render else ""

  def reverseForeach(f: A => Unit): Unit = xs match {
    case xs: Direct[A] => xs.indices.reverse foreach (i => f(xs(i)))
    case _             => xs.pvec.reverse foreach f
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

  def contains(x: A)(implicit z: Eq[A]): Boolean                = exists (_ === x)
  def containsByEquals(x: A): Boolean                           = exists (_ == x)
  def containsRef(x: A with Object): Boolean                    = exists (_ id_== x)
  def findOr(p: Predicate[A], alt: => A): A                     = find(p) | alt
  def findOrZero(p: Predicate[A])(implicit z: Zero[A]): A       = find(p) | z.zero
  def indexByEquals(x: A): Index                                = indexAtWhich(_ == x)
  def mapFrom[B](f: A => B)(implicit z: HashEq[B]): ExMap[B, A] = xs map (x => f(x) -> x) pmap
  def mapOnto[B](f: A => B)(implicit z: HashEq[A]): ExMap[A, B] = xs.pset mapOnto f
  def product(implicit z: Products[A]): A                       = xs.foldl(z.one)(z.product)
  def reducel(f: (A, A) => A): A                                = tail.foldl(head)(f)
  def sum(implicit z: Sums[A]): A                               = xs.foldl(z.zero)(z.sum)
  def without(x: A): View[A]                                    = xs filterNot (_ id_== x)

  /** FIXME - zero is a property of the reduction operation, not the type.
   *  For instance if you're folding a bunch of booleans with && then zero is true,
   *  but with || zero is false.
   */
  def safeReduce(f: (A, A) => A)(implicit z: Zero[A]): A = if (isEmpty) z.zero else reducel(f)

  def distinctBy[B: HashEq](f: A => B): View[A] = inView { mf =>
    var seen: ExSet[B] = exSet[B]()
    xs foreach { x =>
      val y = f(x)
      if (!seen(y)) {
        seen = seen add y
        mf(x)
      }
    }
  }
}

final class WeakApiViewOps[A](val xs: View[A]) extends AnyVal with InvariantViewOps[A] {
  def boundedClosure(maxDepth: Precise, f: A => View[A]): View[A] =
    if (maxDepth.isZero) xs else xs ++ (xs flatMap f).boundedClosure(maxDepth - 1, f)
}

final class PairViewOps[R, A, B](val xs: View[R])(implicit paired: PairDown[R, A, B]) {
  // We should be able to write these method on the normal ViewOps and take the implicit
  // parameter here, like this:
  //   def mapPairs[B, C, D](f: (B, C) => D)(implicit z: Paired[A, B, C]): View[D] = xs map (x => f(z left x, z right x))
  // But scala's type inference sucks so it doesn't work without annotating the parameter types.
  def mapPairs[C](f: (A, B) => C): View[C]                                  = xs map (x => f(paired left x, paired right x))
  def mapLeft[R1, A1](f: A => A1)(implicit z: PairUp[R1, A1, B]): View[R1]  = xs map (x => z.create(f(paired left x), paired right x))
  def mapRight[R1, B1](f: B => B1)(implicit z: PairUp[R1, A, B1]): View[R1] = xs map (x => z.create(paired left x, f(paired right x)))
}
