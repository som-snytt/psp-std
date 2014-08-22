package psp
package core

import psp.std._

final class IndexedConversions[A](val xs: Direct[A]) extends AnyVal { }

final class ExtraViewOperations[A, B, Repr, CC[X]](val xs: ViewEnvironment[A, Repr, CC]#View[B]) extends AnyVal {
  def mapWithIndex[C](f: (B, Int) => C)(implicit pcb: Builds[C, CC[C]]): ViewEnvironment[A, Repr, CC]#View[C] = {
    var i = 0
    xs map (x => try f(x, i) finally i += 1)
  }
}

final class ForeachOperations[A](val xs: Foreach[A]) extends AnyVal {
  /** Abort traversal if/when f returns true.
   */
  @inline final def foreachWithIndex(f: (A, Index) => Done): Unit = {
    var i = 0
    xs foreach (x => if (f(x, index(i))) return else i += 1)
  }
  def sum(implicit num: Numeric[A]): A     = foldl(num.zero)(num.plus)
  def product(implicit num: Numeric[A]): A = foldl(num.one)(num.times)
  def min(implicit ord: Order[A]): A       = reduce(_ min _)
  def max(implicit ord: Order[A]): A       = reduce(_ max _)

  def forcedSize: Long = xs.sizeInfo.precisely match {
    case Some(x) => x
    case _       => foldl(0L)((res, x) => res + 1)
  }

  def reduceLeft[A1 >: A](f: (A1, A) => A1): A1 = reduce[A1](f)
  def reduce[A1 >: A](f: (A1, A) => A1): A1 = {
    var result: A1 = nullAs[A1]
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
  def scanl[B](z: B)(op: (B, A) => B): Foreach[B] = Foreach[B](f => {
    var acc = z
    f(z)
    xs foreach { x =>
      acc = op(acc, x)
      f(acc)
    }
  })

  private def stringed(sep: String)(f: A => String): String =
    foldl(new StringBuilder)((sb, x) => if (sb.isEmpty) sb append f(x) else sb append sep append f(x) ).result

  def joinLines(implicit shows: Show[A]): String         = join(EOL)
  def joinComma(implicit shows: Show[A]): String         = join(", ")
  def join(sep: String)(implicit shows: Show[A]): String = stringed(sep)(_.to_s)
  def mkString(sep: String): String                      = stringed(sep)(_.try_s)

  def find(p: Predicate[A]): Option[A] = { xs.foreach(x => if (p(x)) return Some(x)) ; None }
  def forall(p: Predicate[A]): Boolean = { xs.foreach(x => if (!p(x)) return false) ; true }
  def exists(p: Predicate[A]): Boolean = { xs.foreach(x => if (p(x)) return true) ; false }

  def head: A                                       = find(_ => true) getOrElse failEmpty("head")
  def toArray(implicit ctag: ClassTag[A]): Array[A] = to[Array](Builds wrap Array.canBuildFrom[A])
  def toVector: Vector[A]                           = to[Vector]
  def toList: List[A]                               = to[List]
  def toSeq: Seq[A]                                 = to[Seq]
  def toScalaSet: Set[A]                            = to[Set]
  def toStream: Stream[A]                           = to[Stream]
  def toIterable: Iterable[A]                       = to[Iterable]
  def toTraversable: Traversable[A]                 = ToScala(xs)
  def trav: Traversable[A]                          = toTraversable
  def scalaIterator: Iterator[A]                    = toIterable.iterator

  def buildInto[To](cbf: CanBuildFrom[_, A, To]): To = cbf() ++= toTraversable result

  def toRepr[Repr](implicit pcb: Builds[A, Repr]): Repr = pcb build xs
  def to[CC[X]](implicit pcb: Builds[A, CC[A]]): CC[A]  = pcb build xs

  def toIndexed: Direct[A] = xs match {
    case xs: Direct[A] => xs
    case _              => Direct.elems(toSeq: _*)
  }
  def toPspList: PspList[A] = xs match {
    case xs: PspList[A] => xs
    case _              => to[PspList]
  }
}
