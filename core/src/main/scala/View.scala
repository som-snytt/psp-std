package psp
package core

/** A view class, the product of too many iterations to count.
 */
final class View[+Coll, +A](xs: Coll)(f: Coll => Traversable[A]) {
  private[this] type This               = View[Coll, A]
  private[this] type MapTo[+B]          = View[Coll, B]
  private[this] type Builds[That]       = CBF[Coll, A, That]
  private[this] type CCMap[CC[X], A, B] = CC[A] => CC[B]

  @inline def foreach[@specialized(Unit) U](g: A => U): Unit = f(xs) foreach g
  def withFilter(p: A => Boolean): This                      = filter(p)
  def filter(p: A => Boolean): This                          = transform(_ filter p)
  def map[B](g: A => B): MapTo[B]                            = transform(_ map g)
  def flatMap[B](g: A => GenTraversableOnce[B]): MapTo[B]    = transform(_ flatMap g)
  def transform[B](g: CCMap[Traversable, A, B]): MapTo[B]    = View(xs)(f andThen g)

  def empty: This        = transform[A](_ => Nil)
  def take(n: Int): This = if (n <= 0) empty else transform(_ take n)
  def drop(n: Int): This = if (n <= 0) this else transform(_ drop n)

  // The way in, and the way out.
  def m: this.type                                 = this
  def force[That](implicit cb: Builds[That]): That = cb(xs) ++= f(xs) result
  def finish[B](g: Traversable[A] => B): B         = g(f(xs))
  override def toString = s"view of a ${xs.shortClass}"
}

object View {
  def apply[Coll, A](xs: Coll)(f: Coll => Traversable[A]): View[Coll, A] = new View[Coll, A](xs)(f)
}
