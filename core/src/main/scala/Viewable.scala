package psp
package core

/** Profound language failure at providing the necessary abstractions.
 *  If you want to compare your own collections side-by-side with scala's,
 *  but don't wish to duplicate the signatures verbatim, you are screwed.
 *  This layer is the best I could manage.
 */
trait Viewable[+A] {
  type CC[+X] <: Viewable[X]
  protected[this] type Self = CC[A]

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): CC[B]
  def flatMap[B](f: A => Foreach[B]): CC[B]
  def collect[B](pf: PartialFunction[A, B]): CC[B]
  def filter(p: A => Boolean): Self
  def filterNot(p: A => Boolean): Self
  def slice(start: Int, end: Int): Self
  def take(n: Int): Self
  def drop(n: Int): Self
  def dropRight(n: Int): Self
  def takeRight(n: Int): Self
  def takeWhile(p: A => Boolean): Self
  def dropWhile(p: A => Boolean): Self
  def mkString(sep: String): String
}

/** Implementation of the above which wraps scala collections directly.
 *  My collections implement Viewable in PspView.
 */
final case class ScalaNative[+A](xs: Iterable[A]) extends Viewable[A] {
  type CC[+X] = ScalaNative[X]

  private implicit def lift[B](result: Iterable[B]): CC[B] = ScalaNative(result)

  def foreach(f: A => Unit): Unit                  = xs foreach f
  def map[B](f: A => B): CC[B]                     = xs map f
  def flatMap[B](f: A => Foreach[B]): CC[B]        = xs flatMap (x => f(x).toTraversable.seq)
  def collect[B](pf: PartialFunction[A, B]): CC[B] = xs collect pf
  def filter(p: A => Boolean): Self                = xs filter p
  def filterNot(p: A => Boolean): Self             = xs filterNot p
  def slice(start: Int, end: Int): Self            = xs.slice(start, end)
  def take(n: Int): Self                           = xs take n
  def drop(n: Int): Self                           = xs drop n
  def dropRight(n: Int): Self                      = xs dropRight n
  def takeRight(n: Int): Self                      = xs takeRight n
  def takeWhile(p: A => Boolean): Self             = xs takeWhile p
  def dropWhile(p: A => Boolean): Self             = xs dropWhile p
  def mkString(sep: String): String                = xs mkString sep

  override def toString = s"<scala>.${xs.shortClass}"
}
