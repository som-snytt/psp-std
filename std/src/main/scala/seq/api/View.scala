package psp
package std
package api

trait View[+A] extends Any with Foreach[A] with View.Iso[A] {
  type MapTo[+X] <: View[X]

  def calls: Int
  def description: String
  def viewChain: List[View[_]]

  def ++[A1 >: A](that: Foreach[A1]): MapTo[A1]
  def map[B](f: A => B): MapTo[B]
  def flatten[B](implicit ev: A <:< Foreach[B]): MapTo[B]
  def flatMap[B](f: A => Foreach[B]): MapTo[B]
  def collect[B](pf: A ?=> B): MapTo[B]
}

object View {
  trait Atomic[+A] extends Any with View[A] {
    def m: Atomic[A]
  }
  trait Composite[+A] extends Any with View[A] {
    def prev: View[_]
  }
  trait Builder[+A, Repr] extends Any with View[A] {
    type MapTo[+X] <: Builder[X, Repr]

    def native(implicit z: Builds[A, Repr]): Repr
    def force[That](implicit z: Builds[A, That]): That
  }
  trait Iso[+A] extends Any {
    self: View[A] =>

    private[this] type This = MapTo[A]

    def drop(n: Int): This
    def dropRight(n: Int): This
    def dropWhile(p: Predicate[A]): This
    def filter(p: Predicate[A]): This
    def filterNot(p: Predicate[A]): This
    def labeled(label: String): This
    def sized(size: Size): This
    def slice(range: IndexRange): This
    def take(n: Int): This
    def takeRight(n: Int): This
    def takeWhile(p: Predicate[A]): This
    def withFilter(p: Predicate[A]): This
  }
}
