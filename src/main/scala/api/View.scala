package psp
package core
package api

trait View[+A] extends Any with TypeConstructors[A] with IsoView[A] with MapElementView[A] with MeasurementView {
  type MapTo[+X] <: View[X]

  // Defined here for the moment out of expedience.
  type Input[+X] = Foreach[X]
  type Split[+X] <: (MapTo[A], MapTo[A])
  def ++[A1 >: A](that: Input[A1]): MapTo[A1]
}

trait AtomicView[+A] extends Any with View[A] {
  def m: AtomicView[A]
}
trait CompositeView[+A] extends Any with View[A] {
  def prev: api.View[_]
}

trait MeasurementView extends Any {
  def calls: Int
  def viewString(formatter: String => String): String
}

trait BuilderView[+A, Repr] extends Any with View[A] {
  type MapTo[+X] <: BuilderView[X, Repr]

  def native(implicit pcb: PspCanBuild[A, Repr]): Repr
  def force[That](implicit pcb: PspCanBuild[A, That]): That
}

trait TypeConstructors[+A] extends Any with Foreach[A] {
  type MapTo[+X]
  type Split[+X]
  type Input[+X]
  type Joined[+X]
}

trait ClusterView[+A] extends Any with TypeConstructors[A] {
  type Cluster[+X]

  def sliding(size: Size, step: Int): MapTo[Cluster[A]]
  def groupBy[B](f: A => B): MapTo[(B, Cluster[A])]
}

trait MapElementView[+A] extends Any with TypeConstructors[A] {
  def map[B](f: A => B): MapTo[B]
  def flatten[B](implicit ev: A <:< Input[B]): MapTo[B]
  def flatMap[B](f: A => Input[B]): MapTo[B]
  def collect[B](pf: A =?> B): MapTo[B]
}

trait IsoView[+A] extends Any with TypeConstructors[A] {
  private[this] type This = MapTo[A]

  def withFilter(p: Predicate[A]): This
  def filter(p: Predicate[A]): This
  def filterNot(p: Predicate[A]): This
  def drop(n: Int): This
  def take(n: Int): This
  def takeWhile(p: Predicate[A]): This
  def dropWhile(p: Predicate[A]): This
  def dropRight(n: Int): This
  def takeRight(n: Int): This
  def slice(range: Interval): This
  def labeled(label: String): This
  def sized(size: Size): This
  def reverse: This
}

trait SplitView[A] extends Any with TypeConstructors[A] {
  def span(p: Predicate[A]): Split[A]
  def partition(p: Predicate[A]): Split[A]
  def splitAt(p: Predicate[A]): Split[A]
}

trait ZipView[+A] extends Any with TypeConstructors[A] {
  def zip[B](that: Input[B]): Joined[B]
  def zipWith[B, C](that: Input[B])(f: (A, B) => C): MapTo[C]
  def corresponds[B](that: Input[B])(p: Predicate2[A, B]): Boolean
}
