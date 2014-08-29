package psp
package std

object Order {
  import api.Cmp._

  def fromOrdering[A](ord: Ordering[A]): Order[A] = apply[A]((x, y) => difference(ord.compare(x, y)))

  def apply[A](f: (A, A) => Cmp): Order[A] = new internal.OrderClass[A](f)

  def fold(xs: Cmp*): Cmp = xs.toSeq findOr (_ != EQ, EQ)

  def difference(diff: Long): Cmp = if (diff < 0) LT else if (diff > 0) GT else EQ
}
