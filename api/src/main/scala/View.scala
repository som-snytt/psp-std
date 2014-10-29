package psp
package std
package api

import ApiAliases._

trait AnyView[+A] extends Any with Each[A] {
  type MapTo[+X] <: AnyView[X]
  type SplitTo[+X] <: View.Split[X]

  def partition(p: Predicate[A]): SplitTo[A]
}

trait SetView[+A] extends Any with AnyView[A] with ExSet[A] {
  type MapTo[+X] <: SetView[X]
}

trait View[+A] extends Any with AnyView[A] with ExSeq[A] {
  type MapTo[+X] <: View[X]
}

trait InMapView[-K, +V] extends Any with AnyView[V] with InMap[K, V] {
  type CoMapTo[-X] <: InMapView[X, V]
  type MapTo[+X] <: InMapView[K, X]

  def comap[K1](f: K1 => K): CoMapTo[K1]
  def filter(p: Predicate[V]): MapTo[V]
  def map[V1](f: V => V1): MapTo[V1]
}

trait ExSeq[+A] extends Any {
  self: View[A] =>

  def ++[A1 >: A](that: View[A1]): MapTo[A1]
  def collect[B](pf: A ?=> B): MapTo[B]
  def viewOps: Direct[String]
  def drop(n: Precise): MapTo[A]
  def dropIndex(index: Index): MapTo[A]
  def dropRight(n: Precise): MapTo[A]
  def dropWhile(p: Predicate[A]): MapTo[A]
  def filter(p: Predicate[A]): MapTo[A]
  def filterNot(p: Predicate[A]): MapTo[A]
  def flatMap[B](f: A => Each[B]): MapTo[B]
  def map[B](f: A => B): MapTo[B]
  def slice(range: IndexRange): MapTo[A]
  def span(p: Predicate[A]): SplitTo[A]
  def splitAt(index: Index): SplitTo[A]
  def take(n: Precise): MapTo[A]
  def takeRight(n: Precise): MapTo[A]
  def takeWhile(p: Predicate[A]): MapTo[A]
  def withFilter(p: Predicate[A]): MapTo[A]
  def zip[B](that: View[B]): MapTo[(A, B)]
}


object View {
  trait Atomic[+A] extends Any with View[A]
  trait Composite[A, +B] extends Any with View[B] { def prev: View[A] }
  trait Split[+A] extends Any {
    type Single[+A] <: View[A]
    def mapLeft[A1 >: A](f: Single[A1] => Single[A1]): Split[A1]
    def mapRight[A1 >: A](f: Single[A1] => Single[A1]): Split[A1]
    def left: Single[A]
    def right: Single[A]
    def join: Single[A]
    def intersperse: Single[A]
  }
}
