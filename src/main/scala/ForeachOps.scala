package psp
package core

final class IndexedOperations[A](val xs: Indexed[A]) extends AnyVal {
}

final class ForeachOperations[A](val xs: Foreach[A]) extends AnyVal {
  @inline final def foreachWithIndex(f: (A, Index) => Done): Unit = {
    var i = 0
    xs foreach (x => if (f(x, i)) return else i += 1)
  }
  def sum(implicit num: Numeric[A]): A     = foldl(num.zero)(num.plus)
  def product(implicit num: Numeric[A]): A = foldl(num.one)(num.times)
  def min(implicit ord: Ordering[A]): A    = reduce(ord.min)
  def max(implicit ord: Ordering[A]): A    = reduce(ord.max)

  def reduce(f: (A, A) => A): A = {
    var result: A = null.asInstanceOf[A]
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
}

final class IndexedConversions[A](val xs: Indexed[A]) extends AnyVal {
  def toSizedList = xs.toPspList sized xs.size
}

final class ForeachConversions[A](val xs: Foreach[A]) extends AnyVal {
  def toVector: Vector[A]           = to[Vector]
  def toList: List[A]               = to[List]
  def toSeq: Seq[A]                 = to[Seq]
  def toStream: Stream[A]           = toTraversable.toStream
  def toIterable: Iterable[A]       = toTraversable.toIterable
  def toTraversable: Traversable[A] = ForeachAsTraversable(xs)
  def to[CC[X] <: Traversable[X]](implicit cbf: CanBuildFrom[Nothing, A, CC[A]]): CC[A]  = (cbf() ++= toTraversable).result
  def toRepr[Repr <: Traversable[A]](implicit cbf: CanBuildFrom[Nothing, A, Repr]): Repr = (cbf() ++= toTraversable).result

  // def labeled(label: String): LabeledForeach[A] = LabeledForeach(xs, label)

  def sized(size: Size): Foreach[A] = SizedForeach(xs, size)
  def toPspList: PspList[A]         = PspList(toSeq: _*)

  // def toExtensionalSet(equiv: (A, A) => Boolean): ExtensionalSet[A] = new ExtensionalSet(xs, equiv)
}
