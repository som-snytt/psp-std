package psp
package std
package extract

import api._

/** Name-based extraction only exists in 2.11.
 */

final class HeadAndTail[A](val xs: Linear[A]) extends AnyVal {
  def isEmpty = xs.isEmpty
  def get     = _1 -> _2
  def _1      = xs.head
  def _2      = xs.tail
}

final class Cursor[A](val offset: Long) extends AnyVal {
  def isEmpty = offset < 0
  def index   = offset.index
  def next    = new Cursor[A](offset + 1)
}
final case class Opt[A](get: A) extends AnyVal {
  def isEmpty = get == null
  def map[B](f: A => B): Opt[B] = if (isEmpty) this.asInstanceOf[Opt[B]] else new Opt[B](f(get))
}

final class CursorExtractor[A](xs: Direct[A]) {
  type C = Cursor[this.type]

  def isEmpty        = xs.isEmpty
  def start: C       = new C( if (isEmpty) -1 else 0 )
  def contains(c: C) = xs containsIndex c.index
  def apply(c: C): A = xs(c.index)

  object head { def unapply(c: C): Opt[A]      = Opt( if (contains(c)) xs(c.index) else nullAs[A] ) }
  object tail { def unapply(c: C): C           = if (contains(c)) c.next else new C(-1) }
  object ::   { def unapply(c: C): Opt[(A, C)] = head unapply c map (h => ((h, tail unapply c))) }
}

object :: {
  def unapply[A](xs: Linear[A]): HeadAndTail[A] = new HeadAndTail(xs)
}
