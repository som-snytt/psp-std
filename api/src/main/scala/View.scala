package psp
package std
package api

import ApiAliases._

trait AnyView[+A] extends Any with Each[A] {
  type MapTo[+X] <: AnyView[X]

  def partition(p: Predicate[A]): SplitView[A]
}

trait SetView[A] extends Any with AnyView[A] with ExSet[A] {
}

trait View[+A] extends Any with AnyView[A] {
  type MapTo[+X] <: View[X]

  def ++[A1 >: A](that: View[A1]): MapTo[A1]
  def collect[B](pf: A ?=> B): MapTo[B]
  def viewOps: Direct[String]
  def drop(n: Precise): MapTo[A]
  def dropIndex(index: Index): MapTo[A]
  def dropRight(n: Precise): MapTo[A]
  def dropWhile(p: Predicate[A]): MapTo[A]
  def flatMap[B](f: A => Each[B]): MapTo[B]
  def map[B](f: A => B): MapTo[B]
  def span(p: Predicate[A]): SplitView[A]
  def splitAt(index: Index): SplitView[A]
  def take(n: Precise): MapTo[A]
  def takeRight(n: Precise): MapTo[A]
  def takeWhile(p: Predicate[A]): MapTo[A]
  def withFilter(p: Predicate[A]): MapTo[A]
}

trait InMapView[-K, +V] extends Any with AnyView[V] with InMap[K, V] {
  type CoMapTo[-X] <: InMapView[X, V]
  type MapTo[+X] <: InMapView[K, X]

  def comap[K1](f: K1 => K): CoMapTo[K1]
  def filter(p: Predicate[V]): MapTo[V]
  def map[V1](f: V => V1): MapTo[V1]
}

trait SplitView[+A] extends Any {
  type Single[+A] <: View[A]
  def mapLeft[A1 >: A](f: Single[A1] => Single[A1]): SplitView[A1]
  def mapRight[A1 >: A](f: Single[A1] => Single[A1]): SplitView[A1]
  def left: Single[A]
  def right: Single[A]
  def join: Single[A]
  def intersperse: Single[A]
}
