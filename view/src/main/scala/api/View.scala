package psp
package core
package api

import psp.std._

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
  def description: String
  def viewChain: List[View[_]]
}

trait BuilderView[+A, Repr] extends Any with View[A] {
  type MapTo[+X] <: BuilderView[X, Repr]

  def native(implicit pcb: Builds[A, Repr]): Repr
  def force[That](implicit pcb: Builds[A, That]): That
}

trait TypeConstructors[+A] extends Any with Foreach[A] {
  type MapTo[+X]
  type Split[+X]
  type Input[+X]
  type Joined[+X]
}

trait MapElementView[+A] extends Any with TypeConstructors[A] {
  def map[B](f: A => B): MapTo[B]
  def flatten[B](implicit ev: A <:< Input[B]): MapTo[B]
  def flatMap[B](f: A => Input[B]): MapTo[B]
  def collect[B](pf: A ?=> B): MapTo[B]
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
  def slice(range: IndexRange): This
  def labeled(label: String): This
  def sized(size: Size): This
  def reverse: This
}
