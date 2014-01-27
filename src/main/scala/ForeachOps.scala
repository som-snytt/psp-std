package psp
package core

final class IndexedOperations[A](val xs: Indexed[A]) extends AnyVal {
}

final class ForeachOperations[A](val xs: Foreach[A]) extends AnyVal {
  def hasPreciseSize = xs.sizeInfo.precisely.isDefined
  @inline final def foreachWithIndex(f: (A, Index) => Done): Unit = {
    var i = 0
    xs foreach (x => if (f(x, i)) return else i += 1)
  }
  def sum(implicit num: Numeric[A]): A     = foldl(num.zero)(num.plus)
  def product(implicit num: Numeric[A]): A = foldl(num.one)(num.times)
  def min(implicit ord: Ordering[A]): A    = reduce(ord.min)
  def max(implicit ord: Ordering[A]): A    = reduce(ord.max)

  def reduce(f: (A, A) => A): A = {
    var result: A = nullAs[A]
    var first = true
    xs foreach (x => if (first) try result = x finally first = false else result = f(result, x))
    if (first) failEmpty("reduce") else result
  }
  def foldl[B](zero: B)(f: (B, A) => B): B          = {
    var result = zero
    xs.foreach(x => result = f(result, x))
    result
  }
  def foldr[B](zero: B)(f: (A, B) => B): B          = {
    var result = zero
    xs.foreach(x => result = f(x, result))
    result
  }
  def find(p: A => Boolean): Option[A] = { xs.foreach(x => if (p(x)) return Some(x)) ; None }
  def forall(p: A => Boolean): Boolean = { xs.foreach(x => if (!p(x)) return false) ; true }
  def exists(p: A => Boolean): Boolean = { xs.foreach(x => if (p(x)) return true) ; false }
  def head: A = find(_ => true) getOrElse failEmpty("head")
  def mkString(sep: String): String = foldl(new StringBuilder)(_ append sep append _.to_s).result stripPrefix sep
}

final class IndexedConversions[A](val xs: Indexed[A]) extends AnyVal {
  // def toSizedList = xs.toPspList sized xs.size
}

final class ForeachConversions[A](val xs: Foreach[A]) extends AnyVal {
  def toVector: Vector[A]           = to[Vector]
  def toList: List[A]               = to[List]
  def toSeq: Seq[A]                 = to[Seq]
  def toScalaSet: Set[A]            = to[Set]
  def toStream: Stream[A]           = to[Stream]
  def toIterable: Iterable[A]       = to[Iterable]
  def toTraversable: Traversable[A] = new ForeachAsTraversable(xs)

  def toRepr[Repr](implicit pcb: PspCanBuild[A, Repr]): Repr      = pcb build xs
  def to[CC[X]](implicit pcb: PspCanBuild[A, CC[A]]): CC[A]       = pcb build xs

  def toIndexed: Indexed[A]                                       = Indexed.elems(toSeq: _*)
  def toPspList: PspList[A]                                       = to[PspList]
  def toUniversalSet: EquivSet[A]                                 = EquivSet universal xs
  def toReferenceSet(implicit ev: A <:< Ref[A]): EquivSet[Ref[A]] = EquivSet.reference[Ref[A]](xs map (x => ev(x)))
  def toExtensionalSet: ExtensionalSet[A]                         = new ExtensionalSet(xs)
  def toIntensionalSet: IntensionalSet[A]                         = IntensionalSet(xs)
}
