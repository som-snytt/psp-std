package psp
package std

import scala.collection.immutable

final class ForeachOperations[A](val xs: Foreach[A]) extends AnyVal {
  def sum(implicit num: Numeric[A]): A     = foldl(num.zero)(num.plus)
  def product(implicit num: Numeric[A]): A = foldl(num.one)(num.times)

  def foldl[B](zero: B)(f: (B, A) => B): B = {
    var result = zero
    xs.foreach(x => result = f(result, x))
    result
  }
  def foldr[B](zero: B)(f: (A, B) => B): B          = {
    var result = zero
    xs.foreach(x => result = f(x, result))
    result
  }

  private def stringed(sep: String)(f: A => String): String =
    foldl(new StringBuilder)((sb, x) => if (sb.isEmpty) sb append f(x) else sb append sep append f(x) ).result

  def joinLines(implicit shows: Show[A]): String         = join(EOL)
  def joinComma(implicit shows: Show[A]): String         = join(", ")
  def join(sep: String)(implicit shows: Show[A]): String = stringed(sep)(_.to_s)
  def mkString(sep: String): String                      = stringed(sep)(_.try_s)

  def find(p: Predicate[A]): Option[A] = { xs.foreach(x => if (p(x)) return Some(x)) ; None }
  def forall(p: Predicate[A]): Boolean = { xs.foreach(x => if (!p(x)) return false) ; true }
  def exists(p: Predicate[A]): Boolean = { xs.foreach(x => if (p(x)) return true) ; false }

  def toArray(implicit z: ClassTag[A]): Array[A]    = to[Array]
  def toVector: Vector[A]                           = to[Vector]
  def toList: List[A]                               = to[List]
  def toSeq: Seq[A]                                 = to[Seq]
  def toSet(implicit equiv: HashEq[A]): EquivSet[A] = EquivSet[A](xs)
  def toScalaSet: Set[A]                            = to[Set]
  def toStream: Stream[A]                           = to[Stream]
  def toIterable: Iterable[A]                       = to[Iterable]
  def toTraversable: Traversable[A]                 = new Foreach.ToScala[A](xs)
  def trav: Traversable[A]                          = toTraversable
  def scalaIterator: Iterator[A]                    = toIterable.iterator
  def toIndexed: Direct[A]                          = to[Direct]
  def toPspList: PspList[A]                         = to[PspList]

  def toRepr[Repr](implicit z: Builds[A, Repr]): Repr = z build xs
  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A]  = z build xs
}
