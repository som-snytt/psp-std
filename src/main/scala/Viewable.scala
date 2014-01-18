package psp
package core

/** Profound language failure at providing the necessary abstractions.
 *  If you want to compare your own collections side-by-side with scala's,
 *  but don't wish to duplicate the signatures verbatim, you are screwed.
 *  This layer is the best I could manage.
 */
trait Viewable[A] {
  type CC[X] <: Viewable[X]
  protected[this] type Self = CC[A]

  def sizeInfo: SizeInfo
  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): CC[B]
  def flatMap[B](f: A => Foreach[B]): CC[B]
  def ++(that: Foreach[A]): CC[A]
  // def ++[A1 >: A](that: Foreach[A1]): CC[A1]
  def filter(p: A => Boolean): Self
  def slice(start: Int, end: Int): Self
  def drop(n: Int): Self
  def take(n: Int): Self
  def dropRight(n: Int): Self
  def takeRight(n: Int): Self
}

/** Implementation of the above which wraps scala collections directly.
 *  My collections implement Viewable in PspView.
 */
final case class ScalaNative[A](xs: Iterable[A]) extends Viewable[A] {
  type CC[X] = ScalaNative[X]

  private implicit def lift[B](result: Iterable[B]): CC[B] = ScalaNative(result)

  def sizeInfo: SizeInfo = xs match {
    case xs: IndexedSeq[_] => precise(xs.size)
    case _                 => SizeInfo.Unknown
  }
  def foreach(f: A => Unit): Unit               = xs foreach f
  def map[B](f: A => B): CC[B]                  = xs map f
  def flatMap[B](f: A => Foreach[B]): CC[B]     = xs flatMap (x => f(x).toTraversable.seq)
  def ++(that: Foreach[A]): CC[A]               = xs ++ that.toTraversable.seq
  def filter(p: A => Boolean): Self             = xs filter p
  def slice(start: Int, end: Int): Self         = xs.slice(start, end)
  def drop(n: Int): Self                        = xs drop n
  def take(n: Int): Self                        = xs take n
  def dropRight(n: Int): Self                   = xs dropRight n
  def takeRight(n: Int): Self                   = xs takeRight n

  override def toString = ss"[native:${xs.shortClass}]"
}
